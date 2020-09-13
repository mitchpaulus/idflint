using System.Text;
using System;
using Antlr4.Runtime.Tree;
using dotnet;

namespace tests
{
    public class IddListener : IddBaseListener
    {
        public readonly StringBuilder Builder = new StringBuilder();

        private string _currentObjectName;
        private bool _isUnique = false;
        private IdfObjectFormat _format = IdfObjectFormat.NotSpecified;
        private bool _obsolete = false;
        private int _minNumberOfFields = -1;
        private bool _required = false;

        public override void EnterObject(IddParser.ObjectContext context)
        {
            _currentObjectName = context.object_header().OBJECT_NAME().GetText();

            // Set defaults
            _isUnique = false;
            _obsolete = false;
            _required = false;
            _minNumberOfFields = -1;
            _format = IdfObjectFormat.NotSpecified;
            
            // foreach (IParseTree objectProperty in context.object_header().object_properties().children)
            // {
            // }


            // Builder.Append("    {");
            //
            // Builder.Append($" \"{name}\", new IdfObject({name},  ) ");
            //
            // Builder.Append('\n');
        }

        public override void EnterObject_property(IddParser.Object_propertyContext context)
        {
            if (context.UNIQUE_OBJECT_STATEMENT() != null) _isUnique = true;
            if (context.OBSOLETE_STATEMENT() != null) _obsolete = true;
            if (context.min_fields_statement() != null)
                _minNumberOfFields = int.Parse(context.min_fields_statement().INTEGER().GetText());
            if (context.REQUIRED_OBJECT_STATEMENT() != null) _required = true;
        }

        public override void ExitObject(IddParser.ObjectContext context)
        {
            Builder.Append($"    {{\"{_currentObjectName}\", new IdfObject(\"{_currentObjectName}\", {_isUnique.ToString().ToLower()}, IdfObjectFormat.{IdfObjectFormat.NotSpecified}, {_obsolete.ToString().ToLower()}, {_minNumberOfFields}, {_required.ToString().ToLower()}) }},\n");
        }
    }
}