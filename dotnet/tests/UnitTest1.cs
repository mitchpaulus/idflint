using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using dotnet;
using dotnet.checks;
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
        public void TestMinNumberOfFields()
        {
            string idf = "ZoneAirMassFlowConservation,No;";
            AssertError(idf, typeof(MinNumberOfFieldsError));
        }

        [Test]
        public void TestFieldNotInChoiceError()
        {
            string idf = "PerformancePrecisionTradeoffs,BadChoice;";
            
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            
            Assert.IsTrue(errors.Any(error => error.GetType() == typeof(FieldNotInChoiceError)));
        }

        [Test]
        public void TestObjectTypeNotFoundError()
        {
            string idf = "TotallyUnknownObjectType,No,Yes;";
            AssertError(idf, typeof(ObjectTypeNotFoundError));
        }

        [Test]
        public void TestTooManyFields()
        {
            string idf = "Version,9.2,Another Field;";
            AssertError(idf, typeof(TooManyFieldsProvidedError));
        }

        [Test]
        public void TestNumericFieldIsNumeric()
        {
            string idf = "Timestep,Not a Number;";
            AssertError(idf, typeof(NumericFieldNotNumericError), true);
        }

        public void AssertError(string idf, Type expectedErrorType, bool writeErrors = false)
        {
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsTrue(errors.Any(error => error.GetType() == expectedErrorType));
            if (writeErrors) errors.WriteErrors();
        }
    }
}