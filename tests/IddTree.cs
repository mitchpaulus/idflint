using System.IO;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

namespace tests
{
    public static class IddTree
    {
        public static IddParser.IddContext Tree()
        {
            TextReader reader = new StreamReader("/usr/local/EnergyPlus-9-4-0/Energy+.idd");

            AntlrInputStream input = new AntlrInputStream(reader);

            IddLexer lexer = new IddLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IddParser parser = new IddParser(tokens);
            return parser.idd();
        }
    }
}