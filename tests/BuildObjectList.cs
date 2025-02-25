using System.Collections.Generic;
using System.IO;
using System.Linq;
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
            TextReader reader = new StreamReader("C:\\EnergyPlusV24-2-0\\Energy+.idd");

            AntlrInputStream input = new AntlrInputStream(reader);

            IddLexer lexer = new IddLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IddParser parser = new IddParser(tokens);
            var tree = parser.idd();

            IddListener listener = new IddListener();
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
            sb.Append("        public static bool ContainsKey(string key) => Objects.ContainsKey(key);\n");
            sb.Append("        private static readonly Dictionary<string, IdfObject> CachedDictionary = new Dictionary<string, IdfObject>(StringComparer.OrdinalIgnoreCase);\n");
            sb.Append("        public static IdfObject GetIdfObject(string name)\n");
            sb.Append("        {\n");
            sb.Append("            bool inCache = CachedDictionary.TryGetValue(name, out IdfObject idfObject);\n");
            sb.Append("            if (inCache) return idfObject;\n");
            sb.Append("            if (Objects.ContainsKey(name)) {\n");
            sb.Append("                idfObject = Objects[name]();\n");
            sb.Append("                CachedDictionary[name] = idfObject;\n");
            sb.Append("            }\n");
            sb.Append("            else {\n");
            sb.Append("                throw new ArgumentException();\n");
            sb.Append("            }\n");
            sb.Append("            return idfObject;\n");
            sb.Append("        }\n");
            sb.Append("        private static Dictionary<string, Func<IdfObject>> Objects = new Dictionary<string, Func<IdfObject>>(StringComparer.OrdinalIgnoreCase)\n");
            sb.Append("        {\n");
            sb.Append(listener.Builder.ToString());
            sb.Append("        };\n");
            sb.Append("    }\n");
            sb.Append("}\n");

            File.WriteAllText("C:\\Users\\mpaulus\\repos\\idflint\\app\\IdfObjectListV242.cs", sb.ToString());
        }

        [Test]
        public void BuildReferenceClassList()
        {
            TextReader reader = new StreamReader("C:\\EnergyPlusV24-2-0\\Energy+.idd");

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

            File.WriteAllText("C:\\Users\\mpaulus\\repos\\idflint\\app\\IdfReferenceClassListV242.cs", sb.ToString());
        }

        [Test]
        public void BuildAllObjectTypeAntlr()
        {
            var tree = IddTree.Tree("/usr/local/EnergyPlus-9-4-0/Energy+.idd");

            ObjectNameListener listener = new ObjectNameListener();
            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(listener, tree);

            string typeLines = string.Join("", listener.types.Select(s => $"  {s} |\n"));

            string output = $"fragment object_type : \n{typeLines}";

            File.WriteAllText("/home/mitch/repos/idf-plus/antlr/types.g4", output);
        }

        [Test]
        public void BuildDefaultFiles()
        {
            IddParser.IddContext tree = IddTree.Tree("C:\\EnergyPlusV24-2-0\\Energy+.idd");
            ParseTreeWalker walker = new ParseTreeWalker();

            IddListener listener = new IddListener();

            walker.Walk(listener, tree);

            string directory = "C:\\Users\\mpaulus\\idf";

            foreach (var obj in listener.allObjects)
            {
                string filepath = $"{directory}\\{obj.Name.Replace(":", "__")}.idf";
                File.WriteAllText(filepath, obj.PrintDefaultObject());
            }
        }

        public class ObjectNameListener : IddBaseListener
        {
            public List<string> types = new List<string>();
            public override void EnterObject_header(IddParser.Object_headerContext context)
            {
                var name = context.OBJECT_NAME().GetText().Trim();

                List<string> fragments = name.
                    Select(c => char.IsLetter(c) ? char.ToUpper(c).ToString() : $"'{c}'")
                    .ToList();

                types.Add(string.Join(" ", fragments));
            }
        }
    }
}
