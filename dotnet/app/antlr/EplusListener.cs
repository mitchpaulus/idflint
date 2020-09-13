using System;
using System.Collections.Generic;
using System.Linq;
using dotnet;

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
        public List<IdfParseError> errors = new List<IdfParseError>();

        public override void EnterObject(IdfParser.ObjectContext context) {
            string typeName = context.ALPHA().GetText();

            if (!IdfObjectList.Objects.ContainsKey(typeName))
            {
                errors.Add(new IdfParseError(context.ALPHA().Symbol.Line, context.ALPHA().Symbol.Column, $"{typeName} is not a known object type."));
            }
        }
    }
}