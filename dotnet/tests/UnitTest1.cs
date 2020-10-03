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
        public void TestFieldNotFoundInReferenceList()
        {
            string idf = "Schedule:Constant,Constant Schedule,Type not found,1;\n\n";
            AssertError(idf, typeof(FieldNotFoundInReferenceListError));
        }

        [Test]
        public void TestFieldNotFoundInReferenceClassList()
        {
            string idf = "Pipe:Adiabatic,Pipe,Node1,Node2;  Branch,Name,,Not Valid,Pipe,Node1,Node2;";
            AssertError(idf, typeof(FieldNotFoundInReferenceListError));
        }

        [Test]
        public void TestDuplicateNameInReferenceList()
        {
            string idf = "Schedule:Constant,Schedule1,,1;\n\nSchedule:Constant,Schedule1,,1;";
            AssertError(idf, typeof(DuplicateNameInReferenceListError), true);
        }

        [Test]
        public void TestInclusiveMinimum()
        {
            string idf = "Timestep,0;";
            AssertError(idf, typeof(NumericFieldOutOfRangeError));
        }

        [Test]
        public void TestInclusiveMinimumDoesNotThrowOnEquals()
        {
            string idf = "Timestep,1;";

            IdfLinter linter = new IdfLinter(idf);
            Assert.IsTrue(linter.Lint().Count == 0);
        }

        [Test]
        public void TestExclusiveMinimum()
        {
            string idf = "Material:AirGap,Gap,0";
            AssertError(idf, typeof(NumericFieldOutOfRangeError));
        }

        [Test]
        public void TestInclusiveMaximum()
        {
            string idf = "Timestep,100;";
            AssertError(idf, typeof(NumericFieldOutOfRangeError));
        }

        [Test]
        public void TestInclusiveMaximumDoesNotThrowOnEquals()
        {
            string idf = "Timestep,60;";
            IdfLinter linter = new IdfLinter(idf);
            Assert.IsTrue(linter.Lint().Count == 0);
        }

        [Test]
        public void TestExclusiveMaximum()
        {
            string idf = "ZoneControl:Thermostat:OperativeTemperature,Name,Constant,0.9;";
            AssertError(idf, typeof(NumericFieldOutOfRangeError));
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

            var result = linter.GetReferenceLists(idfLintListener.IdfObjects);

            Assert.IsTrue(result.ReferenceList.Count() == 1);

            Assert.IsTrue(result.ReferenceList["ScheduleNames"].Count() == 1);

            Assert.IsTrue(result.ReferenceList["ScheduleNames"].Contains("Test Schedule"));

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