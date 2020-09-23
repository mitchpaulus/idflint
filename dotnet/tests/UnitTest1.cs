using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using dotnet;
using dotnet.checks;
using Idf;
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

        [Test]
        public void TestBuildingReferenceList()
        {
            string idf = "Schedule:Constant,  Test Schedule  ,,5;";

            IdfParser.IdfContext tree = idf.ParseIdf();

            ParseTreeWalker walker = new ParseTreeWalker();

            IdfLintListener idfLintListener = new IdfLintListener();
            walker.Walk(idfLintListener, tree);

            IdfLinter linter = new IdfLinter(idf);

            var referenceList = linter.GetReferenceLists(idfLintListener.IdfObjects);

            Assert.IsTrue(referenceList.Count() == 1);

            Assert.IsTrue(referenceList["ScheduleNames"].Count() == 1);

            Assert.IsTrue(referenceList["ScheduleNames"][0] == "Test Schedule");

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