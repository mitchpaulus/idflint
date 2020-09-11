using System;
using System.Collections.Generic;
using System.Linq;

namespace Idf {

    public class EplusListener : IdfBaseListener
    {
        public EplusListener()
        {
        }

        public override void EnterObject(IdfParser.ObjectContext context) {
            string typeName = context.object_type().GetText();

            var fields = context.fields();

            var myFields = fields.field();
            
            Console.WriteLine(typeName);

            var paddedFields = string.Join(",\n", myFields.ToList().Select(f => $"    {f.GetText()}")) + ";\n\n";
            Console.WriteLine(paddedFields);

        }
    }
}