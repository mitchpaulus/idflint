using System;
using System.Collections.Generic;
using System.Linq;
using dotnet;
using dotnet.checks;

namespace Idf {

    public class EplusListener : IdfBaseListener
    {
        public EplusListener()
        {
        }

        public override void EnterObject(IdfParser.ObjectContext context) {
            string typeName = context.ALPHA().GetText();

            var fields = context.fields();

            var myFields = fields.field();

            Console.WriteLine($"{typeName},");

            var paddedFields = string.Join(",\n", myFields.ToList().Select(f => $"    {f.GetText()}")) + ";\n";
            Console.WriteLine(paddedFields);
        }
    }

    public class IdfLintListener : IdfBaseListener
    {
        public List<IdfError> errors = new List<IdfError>();

        public override void EnterObject(IdfParser.ObjectContext context) {
            string typeName = context.ALPHA().GetText();

            if (!IdfObjectList.Objects.ContainsKey(typeName))
            {
                errors.Add(new ObjectTypeNotFoundError(context.ALPHA().Symbol, typeName));
                // Return early and don't check any of the fields if we don't know what it is.
                return;
            }

            IdfObject idfObject = IdfObjectList.Objects[typeName];

            FieldChecks fieldChecks = new FieldChecks(context, idfObject);

            errors.AddRange(fieldChecks.Errors());
        }
    }


    public class FieldChecks
    {
        private readonly IdfParser.ObjectContext _actualIdfObject;
        private readonly IdfObject _idfObject;

        public FieldChecks(IdfParser.ObjectContext actualIdfObject, IdfObject idfObject)
        {
            _actualIdfObject = actualIdfObject;
            _idfObject = idfObject;
        }

        public List<IdfError> Errors()
        {
            List<IdfError> errors = new List<IdfError>();

            var fields = _actualIdfObject.fields().field();

            // Check for minimum number of fields
            if (_idfObject.MinNumberOfFields != null && fields.Length < _idfObject.MinNumberOfFields)
            {
                errors.Add(new MinNumberOfFieldsError(_actualIdfObject.Start, _idfObject.Name, _idfObject.MinNumberOfFields.Value, fields.Length));
            }

            if (fields.Length > _idfObject.TotalNumberOfDefinedFields)
            {
                errors.Add( new TooManyFieldsProvidedError(_actualIdfObject.Start, _idfObject.Name, _idfObject.TotalNumberOfDefinedFields, fields.Count()));
            }

            List<(IdfParser.FieldContext ActualField, IdfField ExpectedField)> zippedFields = fields
                .Zip(_idfObject.Fields, (context, field) => (ActualField: context, ExpectedField: field)).ToList();

            foreach ((IdfParser.FieldContext actualField, IdfField expectedField) in zippedFields)
            {
                // Check for matching one of the key values for a field
                var trimmedFieldValue = actualField.GetText().Trim();
                
                if (expectedField.Keys.Any())
                {
                    if (!expectedField.Keys.Contains(trimmedFieldValue) && !(string.IsNullOrWhiteSpace(trimmedFieldValue) && expectedField.HasDefault))
                    {
                        errors.Add(new FieldNotInChoiceError(actualField.Start, expectedField.Name, expectedField.Keys, trimmedFieldValue));
                    }
                }

                if (expectedField.AlphaNumeric == IdfFieldAlphaNumeric.Numeric)
                {
                    bool success = double.TryParse(trimmedFieldValue, out double value) || 
                                   (string.Equals(trimmedFieldValue, "autocalculate", StringComparison.OrdinalIgnoreCase) && expectedField.AutoCalculatable) ||
                                   (string.IsNullOrWhiteSpace(trimmedFieldValue) && !string.IsNullOrWhiteSpace(expectedField.Default));
                    if (!success) errors.Add(new NumericFieldNotNumericError(actualField.Start, expectedField.Name, trimmedFieldValue));
                }
            }

            return errors;
        }
    }
}