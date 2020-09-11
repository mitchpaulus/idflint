using System;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Idf;

namespace dotnet
{
    class Program
    {
        static void Main(string[] args)
        {
            AntlrInputStream input = new AntlrInputStream(Console.In);

            IdfLexer lexer = new IdfLexer(input);

            CommonTokenStream tokens = new CommonTokenStream(lexer);

            var parser = new IdfParser(tokens);

            var tree = parser.idf();
            Console.WriteLine(tree.ToStringTree(parser));

            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(new EplusListener(), tree);


        }
    }
}
