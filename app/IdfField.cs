using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using dotnet.checks;

namespace dotnet
{
    public class IdfField
    {
        public bool Required { get; set; } = false;
        public string Units { get; set; } = "";
        public double Minimum { get; set; }
        public double Maximum { get; set; }
        public string Default { get; set; } = null;
        public bool HasDefault => !string.IsNullOrWhiteSpace(Default);
        public bool AutoSizeable { get; set; } = false;
        public bool AutoCalculatable { get; set; } = false;
        public HashSet<string> Keys { get; set; } = new HashSet<string>();
        public IdfFieldAlphaNumeric AlphaNumeric { get; set; } = IdfFieldAlphaNumeric.Alpha;
        public string Name { get; set; } = "";

        public bool ExtensibleBegin { get; set; } = false;

        public List<string> ReferenceClassList { get; set; } = new List<string>();

        public List<string> ReferenceList { get; set; } = new List<string>();

        public List<string> ObjectList { get; set; } = new List<string>();

        public IdfFieldMinMaxType MinType { get; set; } = IdfFieldMinMaxType.None;
        public IdfFieldMinMaxType MaxType { get; set; } = IdfFieldMinMaxType.None;

        public IdfField() { }

        public IdfField(bool required,
                        string units,
                        double minimum,
                        double maximum,
                        string defaultValue,
                        bool autoCalculatable,
                        bool autoSizeable,
                        IdfFieldAlphaNumeric alphaNumeric,
                        HashSet<string> keys,
                        string name,
                        IdfFieldMinMaxType minType,
                        IdfFieldMinMaxType maxType,
                        List<string> referenceList,
                        List<string> referenceClassList,
                        List<string> objectList)
        {
            Required = required;
            Units = units;
            Minimum = minimum;
            Maximum = maximum;
            Default = defaultValue;
            AutoCalculatable = autoCalculatable;
            AutoSizeable = autoSizeable;
            AlphaNumeric = alphaNumeric;
            Keys = keys;
            Name = name;
            MinType = minType;
            MaxType = maxType;
            ReferenceList = referenceList;
            ReferenceClassList = referenceClassList;
            ObjectList = objectList;
        }

        public string WriteConstructor()
        {
            List<string> parameters = new List<string>
            {
                Required.ToBoolString(),
                Units.WrapInQuotes(),
                Minimum.ToString(),
                Maximum.ToString(),
                Default.WrapInQuotes(),
                AutoCalculatable.ToBoolString(),
                AutoSizeable.ToBoolString(),
                $"IdfFieldAlphaNumeric.{AlphaNumeric}",
                WriteKeys(),
                Name.WrapInQuotes(),
                $"IdfFieldMinMaxType.{MinType}",
                $"IdfFieldMinMaxType.{MaxType}",
                WriteStringList(ReferenceList),
                WriteStringList(ReferenceClassList),
                WriteStringList(ObjectList),
            };
            return
                $"new IdfField({string.Join(",", parameters)})";
        }

        private string WriteKeys() => $"new HashSet<string>(StringComparer.OrdinalIgnoreCase){{{Keys.JoinStrings()}}}";

        private string WriteStringList(IEnumerable<string> strings) => $"new List<string>{{{strings.JoinStrings()}}}";

        public string WriteDefaultLine(bool terminate, int fieldNum)
        {
            List<string> options = new List<string>();

            if (!string.IsNullOrWhiteSpace(Units)) options.Add($"{{{Units}}}");
            if (HasDefault) options.Add($"Def: {Default}");
            if (Keys.Any()) options.Add($"[{string.Join(", ", Keys)}]");
            if (ReferenceList.Any()) options.Add($"RefList: [{string.Join(", ", ReferenceList)}]");
            if (ReferenceClassList.Any()) options.Add($"RefClassList: [{string.Join(", ", ReferenceClassList)}]");
            if (ObjectList.Any()) options.Add($"[{string.Join(", ", ObjectList)}]");
            if (AutoCalculatable) options.Add("AC");
            if (AutoSizeable) options.Add("AS");
            if (Required) options.Add("REQ");
            options.Add($"#{fieldNum}");

            return $"  {(HasDefault ? Default : "")}{(terminate ? ";" : ",")}   ! {Name} {string.Join(", ", options)}\n";
        }
    }

    public class IdfObject
    {
        public bool Unique { get; set; } = false;
        public IdfObjectFormat Format { get; set; } = IdfObjectFormat.NotSpecified;
        public bool Obsolete { get; set; } = false;
        public int? MinNumberOfFields { get; set; } = null;
        public bool Required { get; set; } = false;
        public string Name { get; set; } = "";

        public bool Extensible { get; set; } = false;

        public int ExtensibleCountSize { get; set; } = 0;
        public List<IdfField> Fields { get; set; } = new List<IdfField>();

        // For extensible objects, the fields of one extension group. Expected fields
        // past the end of Fields cycle through this group indefinitely.
        public List<IdfField> ExtensionFields { get; set; } = new List<IdfField>();

        // For extensible objects, we aren't go to make new objects for all the possible ones defined,
        // but we do want to check that the user doesn't have to add extra items to the IDD.
        public int TotalNumberOfDefinedFields { get; set; } = 0;

        public IdfObject() { }

        public IdfObject(string name, bool unique, IdfObjectFormat format, bool obsolete, int? minNumberOfFields,
            bool required, List<IdfField> fields, bool extensible, int totalNumberOfDefinedFields)
        {
            Unique = unique;
            Format = format;
            Obsolete = obsolete;
            MinNumberOfFields = minNumberOfFields;
            Required = required;
            Name = name;
            Fields = fields;
            Extensible = extensible;
            TotalNumberOfDefinedFields = totalNumberOfDefinedFields;
        }

        public string WriteObjectConstructor()
        {
            var fields = string.Join(",", Fields.Select(field => field.WriteConstructor()));
            List<string> parameters = new List<string>()
            {
                Name.WrapInQuotes(),
                Unique.ToBoolString(),
                $"IdfObjectFormat.{Format}",
                Obsolete.ToBoolString(),
                MinNumberOfFields == null ? "null" : MinNumberOfFields.Value.ToString(),
                Required.ToBoolString(),
                $"new List<IdfField> {{{fields}}}",
                Extensible.ToBoolString(),
                TotalNumberOfDefinedFields.ToString()
            };

            return $"new IdfObject({string.Join(",", parameters)} )";
        }

        public IdfField ExpectedFieldAt(int index)
        {
            if (index < Fields.Count) return Fields[index];
            if (ExtensionFields.Count == 0) return null;
            return ExtensionFields[(index - Fields.Count) % ExtensionFields.Count];
        }

        public bool TryGetFieldValue(ParsedIdf idf, in RawObject obj, string fieldName, out string value)
        {
            if (string.IsNullOrWhiteSpace(fieldName))
            {
                value = null;
                return false;
            }

            for (int k = 0; k < obj.FieldCount; k++)
            {
                IdfField expectedField = ExpectedFieldAt(k);
                if (expectedField == null) break;
                if (string.Equals(expectedField.Name, fieldName, StringComparison.OrdinalIgnoreCase))
                {
                    value = idf.FieldText(obj.FirstField + k);
                    return true;
                }
            }

            value = null;
            return false;
        }

        protected bool Equals(IdfObject other)
        {
            return string.Equals(Name, other.Name, StringComparison.OrdinalIgnoreCase);
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((IdfObject) obj);
        }

        public override int GetHashCode()
        {
            return (Name != null ? StringComparer.OrdinalIgnoreCase.GetHashCode(Name) : 0);
        }

        public void FieldChecks(ParsedIdf idf, in RawObject obj, List<IdfError> errors)
        {
            int fieldCount = obj.FieldCount;

            // Check for minimum number of fields. EnergyPlus fills omitted trailing fields
            // with their defaults, so falling short of \min-fields only matters when one
            // of the omitted fields is required and has no default.
            if (MinNumberOfFields != null && fieldCount < MinNumberOfFields)
            {
                bool missingRequiredField = false;
                for (int i = fieldCount; i < MinNumberOfFields.Value; i++)
                {
                    IdfField omittedField = ExpectedFieldAt(i);
                    if (omittedField == null) break;
                    if (omittedField.Required && !omittedField.HasDefault)
                    {
                        missingRequiredField = true;
                        break;
                    }
                }

                if (missingRequiredField)
                {
                    errors.Add(new MinNumberOfFieldsError(idf.ObjectPosition(obj), Name, MinNumberOfFields.Value, fieldCount));
                }
            }

            if (fieldCount > TotalNumberOfDefinedFields)
            {
                errors.Add(new TooManyFieldsProvidedError(idf.ObjectPosition(obj), Name, TotalNumberOfDefinedFields, fieldCount));
            }

            for (int k = 0; k < fieldCount; k++)
            {
                IdfField expectedField = ExpectedFieldAt(k);
                if (expectedField == null) break;

                int fieldIndex = obj.FirstField + k;
                ReadOnlySpan<char> value = idf.FieldSpan(fieldIndex);
                bool blankOk = value.IsEmpty && (expectedField.HasDefault || !expectedField.Required);

                // Check for matching one of the key values for a field
                if (expectedField.Keys.Count > 0)
                {
                    if (!expectedField.Keys.GetAlternateLookup<ReadOnlySpan<char>>().Contains(value) && !blankOk)
                    {
                        errors.Add(new FieldNotInChoiceError(idf.FieldPosition(fieldIndex), expectedField.Name, expectedField.Keys, value.ToString()));
                    }
                }

                if (expectedField.AlphaNumeric == IdfFieldAlphaNumeric.Numeric)
                {
                    bool parsesAsDouble = double.TryParse(value, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out double parsed);

                    if (parsesAsDouble)
                    {
                        if (expectedField.MinType == IdfFieldMinMaxType.Inclusive && parsed < expectedField.Minimum)
                            errors.Add(new NumericFieldOutOfRangeError(idf.FieldPosition(fieldIndex), MinMax.Minimum, expectedField.MinType, value.ToString(), expectedField.Minimum, expectedField.Name));
                        else if (expectedField.MinType == IdfFieldMinMaxType.Exclusive && parsed <= expectedField.Minimum)
                            errors.Add(new NumericFieldOutOfRangeError(idf.FieldPosition(fieldIndex), MinMax.Minimum, expectedField.MinType, value.ToString(), expectedField.Minimum, expectedField.Name));
                        else if (expectedField.MaxType == IdfFieldMinMaxType.Inclusive && parsed > expectedField.Maximum)
                            errors.Add(new NumericFieldOutOfRangeError(idf.FieldPosition(fieldIndex), MinMax.Maximum, expectedField.MaxType, value.ToString(), expectedField.Maximum, expectedField.Name));
                        else if (expectedField.MaxType == IdfFieldMinMaxType.Exclusive && parsed >= expectedField.Maximum)
                            errors.Add(new NumericFieldOutOfRangeError(idf.FieldPosition(fieldIndex), MinMax.Maximum, expectedField.MaxType, value.ToString(), expectedField.Maximum, expectedField.Name));
                    }
                    else
                    {
                        bool properlyAutocalculatable = expectedField.AutoCalculatable && value.Equals("autocalculate", StringComparison.OrdinalIgnoreCase);
                        bool properlyAutosizeable = expectedField.AutoSizeable && value.Equals("autosize", StringComparison.OrdinalIgnoreCase);
                        if (!properlyAutocalculatable && !properlyAutosizeable && !blankOk)
                        {
                            errors.Add(new NumericFieldNotNumericError(idf.FieldPosition(fieldIndex), expectedField.Name, value.ToString()));
                        }
                    }
                }
            }
        }

        public string PrintDefaultObject()
        {
            StringBuilder builder = new StringBuilder();

            if (MinNumberOfFields != null) builder.Append($"! Min Fields: {MinNumberOfFields}\n");
            builder.Append($"{Name},\n");

            var index = Fields.FindIndex(field => field.ExtensibleBegin);

            var printFields = index < 0 ? Fields : Fields.Take(index + 1 + ExtensibleCountSize).ToList();

            var fields = printFields.Take(printFields.Count - 1)
                .Select((field, i) => (Fields: field, FieldNum: i + 1) )
                .Select(tuple => tuple.Fields.WriteDefaultLine(false, tuple.FieldNum))
                .ToList();
            foreach (var field in fields) builder.Append(field);
            builder.Append(printFields.Last().WriteDefaultLine(true, printFields.Count));
            return builder.ToString();
        }
    }


    public class IdfUnit
    {


    }

    public enum IdfFieldAlphaNumeric
    {
        Alpha = 0,
        Numeric = 1,
    }




    public enum IdfFieldMinMaxType
    {
        None = 0,
        Inclusive = 1,
        Exclusive = 2
    }

    public enum IdfObjectFormat
    {
        NotSpecified = -1,
        SingleLine = 0,
        Vertices = 1,
        CompactSchedule = 2,
        FluidProperties = 3,
        ViewFactors = 4,
        Spectral = 5,

    }

    public enum IdfFieldType
    {
        Integer = 0,
        Real = 1,
        Alpha = 2,
        Choice = 3,
        ObjectList = 4,
        ExternalList = 5,
        Node = 6,
    }

    public static class StringExtensions
    {
        public static string ToBoolString(this bool value) =>  value ? "true" : "false";

        public static string WrapInQuotes(this string value) => value == null ? "null" : $"\"{value}\"";

        public static string OrList(this IEnumerable<string> values)
        {
            List<string> enumerable = values.ToList();
            return enumerable.Count() switch
            {
                1 => enumerable.First(),
                2 => $"{enumerable.First()} or {enumerable.Last()}",
                _ => enumerable.Count > 2
                    ? $"{string.Join(", ", enumerable.Take(enumerable.Count - 1))}, or {enumerable.Last()}"
                    : string.Empty
            };
        }

        public static string AndList(this IEnumerable<string> values)
        {
            var enumerable = values.ToList();
            return enumerable.Count() == 2 ? $"{enumerable.First()} and {enumerable.Last()}" : $"{string.Join(", ", enumerable.Take(enumerable.Count() - 1))}, and {enumerable.Last()}";
        }
    }


    public static class ErrorExtensions
    {
        public static void WriteErrors(this List<IdfError> errors)
        {
            var groupedErrors = new List<(string Message, List<IdfError> Instances)>();
            var lookup = new Dictionary<string, List<IdfError>>();

            foreach (IdfError error in errors)
            {
                string message = error.Message();

                if (!lookup.TryGetValue(message, out var instances))
                {
                    instances = new List<IdfError>();
                    lookup[message] = instances;
                    groupedErrors.Add((message, instances));
                }

                instances.Add(error);
            }

            foreach (var (message, instances) in groupedErrors)
            {
                var orderedInstances = instances
                    .OrderBy(instance => instance.Line())
                    .ThenBy(instance => instance.Character())
                    .ToList();

                var locations = orderedInstances
                    .Select(instance => $"{instance.Line()}:{instance.Character()}")
                    .ToList();

                if (!locations.Any()) continue;

                string output;

                if (locations.Count == 1)
                {
                    output = $"{locations.First()} {message}";
                }
                else
                {
                    var displayedLocations = locations.Count > 5
                        ? $"{string.Join(", ", locations.Take(5))}, and {locations.Count - 5} other{(locations.Count - 5 == 1 ? "" : "s")}" 
                        : string.Join(", ", locations);

                    output = $"{displayedLocations}. {message}";
                }

                Console.WriteLine(output);
            }
        }
    }
}