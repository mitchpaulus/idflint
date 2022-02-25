using System.IO;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

namespace tests
{
    public static class IddTree
    {
        public static IddParser.IddContext Tree(string iddPath)
        {
            TextReader reader = new StreamReader(iddPath);

            AntlrInputStream input = new AntlrInputStream(reader);

            IddLexer lexer = new IddLexer(input);
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IddParser parser = new IddParser(tokens);
            return parser.idd();
        }
    }
}