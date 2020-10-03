using System.IO;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using dotnet;
using NUnit.Framework;

namespace tests
{
    [TestFixture]
    public class BuildObjectList
    {
        [Test]
        public void BuildIdfObjectList()
        {
            TextReader reader = new StreamReader("/home/mitch/EnergyPlus/idd/V9-2-0-Energy+.idd");

            AntlrInputStream input = new AntlrInputStream(reader);

            IddLexer lexer = new IddLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IddParser parser = new IddParser(tokens);

            IddListener listener = new IddListener();

            var tree = parser.idd();

            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(listener, tree);

            // BuildMyString.com generated code. Please enjoy your string responsibly.

            StringBuilder sb = new StringBuilder();

            sb.Append("using System;\n");
            sb.Append("using System.Collections.Generic;\n");
            sb.Append("namespace dotnet\n");
            sb.Append("{\n");
            sb.Append("    public static class IdfObjectList\n");
            sb.Append("    {\n");
            sb.Append("        public static Dictionary<string, IdfObject> Objects = new Dictionary<string, IdfObject>(StringComparer.OrdinalIgnoreCase)\n");
            sb.Append("        {\n");
            sb.Append(listener.Builder.ToString());
            sb.Append("        };\n");
            sb.Append("    }\n");
            sb.Append("}\n");

            File.WriteAllText("/home/mitch/repos/idf-lint/dotnet/app/IdfObjectList.cs", sb.ToString());
        }

        [Test]
        public void BuildReferenceClassList()
        {
            TextReader reader = new StreamReader("/home/mitch/EnergyPlus/idd/V9-2-0-Energy+.idd");

            AntlrInputStream input = new AntlrInputStream(reader);

            IddLexer lexer = new IddLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IddParser parser = new IddParser(tokens);

            ReferenceClassIddListener listener = new ReferenceClassIddListener();

            var tree = parser.idd();

            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(listener, tree);

            // BuildMyString.com generated code. Please enjoy your string responsibly.

            StringBuilder sb = new StringBuilder();

            sb.Append("using System;\n");
            sb.Append("using System.Collections.Generic;\n");
            sb.Append("namespace dotnet\n");
            sb.Append("{\n");
            sb.Append("    public static class IdfReferenceClassList\n");
            sb.Append("    {\n");
            sb.Append("        public static Dictionary<string, HashSet<string>> List = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase)\n");
            sb.Append("        {\n");
            foreach (var key in listener.ReferenceClasses.Keys)
            {
                sb.Append($"            {{ \"{key}\", new HashSet<string>(StringComparer.OrdinalIgnoreCase) {{ {listener.ReferenceClasses[key].JoinStrings()}}}  }},\n");
            }
            sb.Append("        };\n");
            sb.Append("    }\n");
            sb.Append("}\n");

            File.WriteAllText("/home/mitch/repos/idf-lint/dotnet/app/IdfReferenceClassList.cs", sb.ToString());
        }
    }
}