using System.Text;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using Antlr4.Runtime.Tree;
using dotnet;

namespace tests
{
    public class IddListener : IddBaseListener
    {
        public readonly StringBuilder Builder = new StringBuilder();
        private IdfObject currentObject = new IdfObject();

        private bool _inExtensibleSection = false;
        private int _beginExtensibleIndex = 0;

        private int _currentFieldIndex = -1;

        public override void EnterObject(IddParser.ObjectContext context)
        {
            currentObject = new IdfObject();
            _inExtensibleSection = false;
            _currentFieldIndex = -1;
            currentObject.Name = context.object_header().OBJECT_NAME().GetText().Trim();
            // The plus 1 is for the 'terminating' field.
            currentObject.TotalNumberOfDefinedFields = context.fields().field().Length + 1;
        }

        public override void EnterObject_property(IddParser.Object_propertyContext context)
        {
            if (context.UNIQUE_OBJECT_STATEMENT() != null) currentObject.Unique = true;
            if (context.OBSOLETE_STATEMENT() != null) currentObject.Obsolete = true;
            if (context.min_fields_statement() != null)
                currentObject.MinNumberOfFields = int.Parse(context.min_fields_statement().INTEGER().GetText());
            if (context.REQUIRED_OBJECT_STATEMENT() != null) currentObject.Required = true;
            if (context.EXTENSIBLE_STATEMENT() != null)
            {
                var splitStatement = context.EXTENSIBLE_STATEMENT().GetText().Split(null);
                int extensibleCount = int.Parse(splitStatement[0].Split(':')[1]);
                currentObject.Extensible = true;
                currentObject.ExtensibleCountSize = extensibleCount;
            }
        }

        public override void EnterField(IddParser.FieldContext context)
        {
            _currentFieldIndex++;
            var alphaNumeric = context.field_id().ALPHA_OPTION() != null
                ? IdfFieldAlphaNumeric.Alpha
                : IdfFieldAlphaNumeric.Numeric;

            if (IsStartOfExtensibleField(context.field_properties()))
            {
                _inExtensibleSection = true;
                _beginExtensibleIndex = _currentFieldIndex;
            }

            if (!_inExtensibleSection || _currentFieldIndex < _beginExtensibleIndex + currentObject.ExtensibleCountSize)
            {
                IdfField idfField = GetField(context.field_properties(), alphaNumeric);
                currentObject.Fields.Add(idfField);
            }
        }

        public override void EnterTerminating_field(IddParser.Terminating_fieldContext context)
        {
            var alphaNumeric = context.field_id().ALPHA_OPTION() != null
                ? IdfFieldAlphaNumeric.Alpha
                : IdfFieldAlphaNumeric.Numeric;

            if (IsStartOfExtensibleField(context.field_properties()))
            {
                _inExtensibleSection = true;
                _beginExtensibleIndex = _currentFieldIndex;
            }

            if (!_inExtensibleSection || _currentFieldIndex < _beginExtensibleIndex + currentObject.ExtensibleCountSize)
            {
                IdfField idfField = GetField(context.field_properties(), alphaNumeric);
                currentObject.Fields.Add(idfField);
            }
        }

        public bool IsStartOfExtensibleField(IddParser.Field_propertiesContext context) =>
            context.field_property().Any(propertyContext => propertyContext.BEGIN_EXTENSIBLE_STATEMENT() != null);

        public IdfField GetField(IddParser.Field_propertiesContext context, IdfFieldAlphaNumeric alphaNumeric)
        {
            IdfField field = new IdfField();

            field.AlphaNumeric = alphaNumeric;

            IddParser.Field_propertiesContext propertyContext = context;
            IddParser.Field_propertyContext[] properties = propertyContext.field_property();

            foreach (IddParser.Field_propertyContext prop in properties)
            {
                if (prop.AUTOSIZABLE_STATEMENT() != null) field.AutoSizeable = true;
                else if (prop.AUTOCALCULATABLE_STATEMENT() != null) field.AutoCalculatable = true;
                else if (prop.REQUIRED_FIELD_STATEMENT() != null) field.Required = true;
                else if (prop.UNITS_STATEMENT() != null) field.Units =  prop.UNITS_STATEMENT().GetText().Substring(7).Trim();
                else if (prop.KEY_STATEMENT() != null) field.Keys.Add(prop.KEY_STATEMENT().GetText().Substring(5).Trim());
                else if (prop.FIELD_STATEMENT() != null) field.Name = prop.FIELD_STATEMENT().GetText().Substring(7).Trim();
                else if (prop.REFERENCE_STATEMENT() != null) field.ReferenceList.Add(prop.REFERENCE_STATEMENT().GetText().Substring(11).Trim());
                else if (prop.OBJECT_LIST_STATEMENT() != null) field.ObjectList.Add(prop.OBJECT_LIST_STATEMENT().GetText().Substring(13).Trim());
                else if (prop.minimum_inclusive_statement() != null)
                {
                    field.MinType = IdfFieldMinMaxType.Inclusive;
                    field.Minimum = double.Parse(prop.minimum_inclusive_statement().minval.Text);
                }
                else if (prop.maximum_inclusive_statement() != null)
                {
                    field.MinType = IdfFieldMinMaxType.Inclusive;
                    field.Maximum = double.Parse(prop.maximum_inclusive_statement().maxval.Text);
                }
                else if (prop.DEFAULT_STATEMENT() != null)
                {
                    field.Default = prop.DEFAULT_STATEMENT().GetText().Substring(9).Trim();
                }
            }

            return field;
        }

        public override void EnterField_property(IddParser.Field_propertyContext context)
        {

        }

        public override void ExitObject(IddParser.ObjectContext context)
        {
            Builder.Append($"            {{\"{currentObject.Name}\", {currentObject.WriteObjectConstructor()} }},\n");
        }
    }
}