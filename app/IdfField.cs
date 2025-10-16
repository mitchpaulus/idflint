using System;
using System.Collections.Generic;
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

        public List<BoundField> ZipWithFields(IEnumerable<IdfParser.FieldContext> fields) => Fields.Zip(fields, (field, s) => new BoundField(field, s)).ToList();

        public bool TryGetFieldValue(IdfParser.ObjectContext objectContext, string fieldName, out string value)
        {
            if (objectContext == null) throw new ArgumentNullException(nameof(objectContext));
            if (string.IsNullOrWhiteSpace(fieldName))
            {
                value = null;
                return false;
            }

            var boundFields = ZipWithFields(objectContext.fields().field());
            var matchingField = boundFields.FirstOrDefault(field => string.Equals(field.ExpectedField.Name, fieldName, StringComparison.OrdinalIgnoreCase));

            if (matchingField == null)
            {
                value = null;
                return false;
            }

            value = matchingField.FoundField;
            return true;
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

        public List<IdfError> FieldChecks(IdfParser.ObjectContext actualIdfObject)
        {
            List<IdfError> errors = new List<IdfError>();

            var fields = actualIdfObject.fields().field();

            // Check for minimum number of fields
            if (MinNumberOfFields != null && fields.Length < MinNumberOfFields)
            {
                errors.Add(new MinNumberOfFieldsError(actualIdfObject.Start, Name, MinNumberOfFields.Value, fields.Length));
            }

            if (fields.Length > TotalNumberOfDefinedFields)
            {
                errors.Add( new TooManyFieldsProvidedError(actualIdfObject.Start, Name, TotalNumberOfDefinedFields, fields.Count()));
            }

            List<(IdfParser.FieldContext ActualField, IdfField ExpectedField)> zippedFields = fields
                .Zip(Fields, (context, field) => (ActualField: context, ExpectedField: field)).ToList();

            foreach ((IdfParser.FieldContext actualField, IdfField expectedField) in zippedFields)
            {
                // Check for matching one of the key values for a field
                var trimmedFieldValue = actualField.GetText().Trim();

                if (expectedField.Keys.Any())
                {
                    if (!expectedField.Keys.Contains(trimmedFieldValue) && !(string.IsNullOrWhiteSpace(trimmedFieldValue) && (expectedField.HasDefault || !expectedField.Required)))
                    {
                        errors.Add(new FieldNotInChoiceError(actualField.Start, expectedField.Name, expectedField.Keys, trimmedFieldValue));
                    }
                }

                if (expectedField.AlphaNumeric == IdfFieldAlphaNumeric.Numeric)
                {
                    bool properlyAutocalculatable = (string.Equals(trimmedFieldValue, "autocalculate", StringComparison.OrdinalIgnoreCase) && expectedField.AutoCalculatable);
                    bool properlyAutosizeable =
                        (string.Equals(trimmedFieldValue, "autosize", StringComparison.OrdinalIgnoreCase) &&
                         expectedField.AutoSizeable);
                    bool parsesAsDouble = double.TryParse(trimmedFieldValue, out double value);
                    var isBlankAndNotRequired = (string.IsNullOrWhiteSpace(trimmedFieldValue) && (expectedField.HasDefault || !expectedField.Required));

                    bool success = parsesAsDouble || properlyAutocalculatable || isBlankAndNotRequired || properlyAutosizeable;
                    if (!success) errors.Add(new NumericFieldNotNumericError(actualField.Start, expectedField.Name, trimmedFieldValue));
                    else if (parsesAsDouble)
                    {
                        if (expectedField.MinType == IdfFieldMinMaxType.Inclusive && value < expectedField.Minimum)
                            errors.Add(new NumericFieldOutOfRangeError(actualField.Start,  MinMax.Minimum, expectedField.MinType, actualField.GetText(), expectedField.Minimum, expectedField.Name));
                        else if (expectedField.MinType == IdfFieldMinMaxType.Exclusive && value <= expectedField.Minimum)
                            errors.Add(new NumericFieldOutOfRangeError(actualField.Start,  MinMax.Minimum, expectedField.MinType, actualField.GetText(), expectedField.Minimum, expectedField.Name));
                        else if (expectedField.MaxType == IdfFieldMinMaxType.Inclusive && value > expectedField.Maximum)
                            errors.Add(new NumericFieldOutOfRangeError(actualField.Start,  MinMax.Maximum, expectedField.MaxType, actualField.GetText(), expectedField.Maximum, expectedField.Name));
                        else if (expectedField.MaxType == IdfFieldMinMaxType.Exclusive && value >= expectedField.Maximum)
                            errors.Add(new NumericFieldOutOfRangeError(actualField.Start,  MinMax.Maximum, expectedField.MaxType, actualField.GetText(), expectedField.Maximum, expectedField.Name));
                    }
                }
            }

            return errors;
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

    public class BoundField
    {
        public IdfField ExpectedField;
        public IdfParser.FieldContext FieldContext;
        public string FoundField => FieldContext.GetText().Trim();

        public BoundField(IdfField expectedField, IdfParser.FieldContext fieldContext)
        {
            ExpectedField = expectedField;
            FieldContext = fieldContext;
        }
    }

    public class BoundObject
    {
        public IdfObject ExpectedObject;
        public IdfParser.ObjectContext FoundObject;


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
            foreach (IdfError error in errors) Console.WriteLine($"{error.Line()}:{error.Character()} {error.Message()}");
        }
    }
}