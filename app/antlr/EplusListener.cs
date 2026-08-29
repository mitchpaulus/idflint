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

        // This is a dictionary lookup with the key being the object type name, and the value is the list of associated objects.
        public Dictionary<string, List<IdfParser.ObjectContext>> IdfObjects = new Dictionary<string, List<IdfParser.ObjectContext>>();

        public override void EnterObject(IdfParser.ObjectContext context)
        {
            // Object types are validated later, once the file's Version object has
            // selected which object database to lint against.
            string typeName = context.ALPHA().GetText();
            IdfObjects.AddSafe(typeName, context);
        }
    }
}
