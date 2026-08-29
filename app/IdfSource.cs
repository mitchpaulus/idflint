using System;
using System.Buffers;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace dotnet
{
    /// <summary>A 1-based line and 0-based column, matching the error output format.</summary>
    public readonly struct SourcePosition
    {
        public readonly int Line;
        public readonly int Column;
        public SourcePosition(int line, int column) { Line = line; Column = column; }
    }

    /// <summary>
    /// A field's trimmed text as an offset/length into the source buffer. Fields are
    /// plain 8-byte structs stored in one contiguous array, so walking an object's
    /// fields is a linear scan over memory the prefetcher already has.
    /// </summary>
    public readonly struct RawField
    {
        public readonly int Start;
        public readonly int Length;
        public RawField(int start, int length) { Start = start; Length = length; }
    }

    /// <summary>
    /// An object is a range in the field array: the type name field followed by
    /// FieldCount data fields.
    /// </summary>
    public readonly struct RawObject
    {
        public readonly int NameField;
        public readonly int FieldCount;
        public int FirstField => NameField + 1;
        public RawObject(int nameField, int fieldCount) { NameField = nameField; FieldCount = fieldCount; }
    }

    /// <summary>
    /// The parse result: the original text plus flat struct arrays indexing into it.
    /// No per-token objects, no tree. Line/column positions are computed lazily from
    /// offsets since they are only needed when an error is actually reported.
    /// </summary>
    public sealed class ParsedIdf
    {
        public readonly string Text;
        private readonly RawField[] _fields;
        private readonly RawObject[] _objects;
        public readonly int FieldCount;
        public readonly int ObjectCount;
        public readonly List<IdfParseError> Errors;
        private int[] _lineStarts;

        internal ParsedIdf(string text, RawField[] fields, int fieldCount, RawObject[] objects, int objectCount)
        {
            Text = text;
            _fields = fields;
            FieldCount = fieldCount;
            _objects = objects;
            ObjectCount = objectCount;
            Errors = new List<IdfParseError>();
        }

        public RawObject Object(int index) => _objects[index];

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ReadOnlySpan<char> FieldSpan(int fieldIndex)
        {
            RawField f = _fields[fieldIndex];
            return Text.AsSpan(f.Start, f.Length);
        }

        public string FieldText(int fieldIndex)
        {
            RawField f = _fields[fieldIndex];
            return Text.Substring(f.Start, f.Length);
        }

        public ReadOnlySpan<char> TypeSpan(in RawObject obj) => FieldSpan(obj.NameField);

        public SourcePosition FieldPosition(int fieldIndex) => Position(_fields[fieldIndex].Start);

        public SourcePosition ObjectPosition(in RawObject obj) => FieldPosition(obj.NameField);

        public SourcePosition Position(int offset)
        {
            int[] starts = _lineStarts ??= BuildLineStarts();
            int idx = Array.BinarySearch(starts, offset);
            if (idx < 0) idx = ~idx - 1;
            return new SourcePosition(idx + 1, offset - starts[idx]);
        }

        private int[] BuildLineStarts()
        {
            List<int> starts = new List<int>(Text.Length / 32 + 4) { 0 };
            ReadOnlySpan<char> span = Text;
            int pos = 0;
            while (true)
            {
                int nl = span.IndexOf('\n');
                if (nl < 0) break;
                pos += nl + 1;
                starts.Add(pos);
                span = span.Slice(nl + 1);
            }
            return starts.ToArray();
        }
    }

    /// <summary>
    /// Hand-rolled IDF parser. One forward pass over the char buffer; the scan jumps
    /// between structural characters (',' ';' '!') with vectorized IndexOfAny, so field
    /// content is skipped at SIMD speed rather than inspected char by char.
    /// </summary>
    public static class IdfSourceParser
    {
        private static readonly SearchValues<char> Structural = SearchValues.Create(",;!");

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static bool IsWs(char c) => c == ' ' || c == '\t' || c == '\r' || c == '\n';

        public static ParsedIdf Parse(string text)
        {
            ReadOnlySpan<char> s = text;
            int n = s.Length;

            // Real files average ~45 chars per field (comments included); n/32 leaves
            // headroom for dense files while keeping the up-front allocation small.
            RawField[] fields = new RawField[Math.Max(64, n / 32)];
            int fieldCount = 0;
            RawObject[] objects = new RawObject[Math.Max(16, n / 500)];
            int objectCount = 0;
            List<(int Offset, string Message)> rawErrors = null;

            int i = 0;
            while (true)
            {
                // Between objects: skip whitespace and comment lines.
                while (i < n)
                {
                    char c = s[i];
                    if (IsWs(c)) { i++; continue; }
                    if (c == '!')
                    {
                        int nl = s.Slice(i).IndexOf('\n');
                        i = nl < 0 ? n : i + nl + 1;
                        continue;
                    }
                    break;
                }
                if (i >= n) break;

                if (s[i] == ',' || s[i] == ';')
                {
                    (rawErrors ??= new List<(int, string)>()).Add((i, $"Unexpected '{s[i]}'."));
                    i++;
                    continue;
                }

                // An object: the type name field, then data fields, until ';'.
                int objStart = i;
                int nameFieldIndex = fieldCount;
                bool terminated = false;
                bool atEof = false;

                while (true)
                {
                    int segStart = i;
                    int cs = -1, ce = -1;     // trimmed content bounds of the field
                    bool extra = false;       // content continued after a comment (missing separator)
                    int extraAt = 0;
                    char delim = '\0';

                    while (true)
                    {
                        int rel = s.Slice(i).IndexOfAny(Structural);
                        int stop = rel < 0 ? n : i + rel;

                        // Trim this segment; only the first content segment defines the value.
                        int a = i;
                        while (a < stop && IsWs(s[a])) a++;
                        if (a < stop)
                        {
                            if (cs < 0)
                            {
                                int b = stop;
                                while (IsWs(s[b - 1])) b--;
                                cs = a;
                                ce = b;
                            }
                            else if (!extra)
                            {
                                extra = true;
                                extraAt = a;
                            }
                        }

                        if (rel < 0) { atEof = true; break; }

                        char d = s[stop];
                        if (d == '!')
                        {
                            int nl = s.Slice(stop).IndexOf('\n');
                            if (nl < 0) { i = n; atEof = true; break; }
                            i = stop + nl + 1;
                            continue;
                        }

                        delim = d;
                        i = stop + 1;
                        break;
                    }

                    // A single content chunk may still contain an interior newline
                    // (a missing separator without an intervening comment).
                    if (cs >= 0)
                    {
                        int nlIn = s.Slice(cs, ce - cs).IndexOf('\n');
                        if (nlIn >= 0)
                        {
                            int contAt = cs + nlIn + 1;
                            while (IsWs(s[contAt])) contAt++;
                            if (!extra) { extra = true; extraAt = contAt; }
                            int e2 = cs + nlIn;
                            while (e2 > cs && IsWs(s[e2 - 1])) e2--;
                            ce = e2;
                        }
                    }

                    if (extra)
                    {
                        (rawErrors ??= new List<(int, string)>()).Add((extraAt, "Expected ',' or ';' before this text."));
                    }

                    if (fieldCount == fields.Length) Array.Resize(ref fields, fields.Length * 2);
                    fields[fieldCount++] = cs < 0
                        ? new RawField(Math.Min(segStart, n == 0 ? 0 : n - 1), 0)
                        : new RawField(cs, ce - cs);

                    if (atEof) break;
                    if (delim == ';') { terminated = true; break; }
                    // delim == ',': next field
                }

                if (objectCount == objects.Length) Array.Resize(ref objects, objects.Length * 2);
                objects[objectCount++] = new RawObject(nameFieldIndex, fieldCount - nameFieldIndex - 1);

                if (!terminated)
                {
                    (rawErrors ??= new List<(int, string)>()).Add((objStart, "Object is missing its terminating ';'."));
                }

                if (atEof) break;
            }

            ParsedIdf parsed = new ParsedIdf(text, fields, fieldCount, objects, objectCount);
            if (rawErrors != null)
            {
                foreach ((int offset, string message) in rawErrors)
                {
                    SourcePosition p = parsed.Position(offset);
                    parsed.Errors.Add(new IdfParseError(p.Line, p.Column, message));
                }
            }
            return parsed;
        }
    }
}
