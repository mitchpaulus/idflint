using System;
using System.Collections.Generic;
using System.Linq;

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
        
        public string ReferenceList { get; set; } = null;

        public string ObjectList { get; set; } = null;

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
                        string referenceList,
                        string objectList)
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
                ReferenceList.WrapInQuotes(),
                ObjectList.WrapInQuotes(),
            };
            return
                $"new IdfField({string.Join(",", parameters)})";
        }

        public string WriteKeys()
        {
            string keys = string.Join(",", Keys.Select(s => $"\"{s}\""));
            return $"new HashSet<string>(StringComparer.OrdinalIgnoreCase){{{keys}}}";
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
            var fields = string.Join(",", Fields.Select(field => field.WriteConstructor() ));
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

        public List<BoundField> ZipWithFields(List<string> fields) => Fields.Zip(fields, (field, s) => new BoundField(field, s)).ToList();

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
    }

    public class BoundField
    {
        public IdfField ExpectedField;
        public string FoundField;

        public BoundField(IdfField expectedField, string foundField)
        {
            ExpectedField = expectedField;
            FoundField = foundField;
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
            var enumerable = values.ToList();
            return enumerable.Count() == 2 ? $"{enumerable.First()} or {enumerable.Last()}" : $"{string.Join(", ", enumerable.Take(enumerable.Count() - 1))}, or {enumerable.Last()}";
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
            foreach (IdfError error in errors) Console.WriteLine($"{error.Line()}:{error.Character()} {error.Message()}");
        }
    }
}