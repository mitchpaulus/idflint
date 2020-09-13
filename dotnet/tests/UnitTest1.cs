using System;
using System.IO;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using dotnet;
using NUnit.Framework;

namespace tests
{
    public class Tests
    {
        [SetUp]
        public void Setup()
        {
        }

        [Test]
        public void Test1()
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
            
            File.WriteAllText("/home/mitch/repos/idf-lint/idd_output.txt", listener.Builder.ToString());
            
        }
    }
}