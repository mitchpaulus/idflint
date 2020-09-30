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

        public Dictionary<string, List<IdfParser.ObjectContext>> IdfObjects = new Dictionary<string, List<IdfParser.ObjectContext>>();

        public override void EnterObject(IdfParser.ObjectContext context) {
            string typeName = context.ALPHA().GetText();

            if (!IdfObjectList.Objects.ContainsKey(typeName))
            {
                errors.Add(new ObjectTypeNotFoundError(context.ALPHA().Symbol, typeName));
                // Return early and don't check any of the fields if we don't know what it is.
                return;
            }

            IdfObjects.AddSafe(typeName, context);

            IdfObject idfObject = IdfObjectList.Objects[typeName];
        }
    }
}