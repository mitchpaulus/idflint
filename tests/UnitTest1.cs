using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
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
        public void TestMinNumberOfFields()
        {
            string idf = "BuildingSurface:Detailed,South Wall;";
            AssertError(idf, typeof(MinNumberOfFieldsError));
        }

        [Test]
        public void TestMinNumberOfFieldsNotReportedWhenOmittedFieldsHaveDefaults()
        {
            // ZoneAirMassFlowConservation has \min-fields 3, but every omitted field
            // has a default, so EnergyPlus accepts this without complaint.
            string idf = "ZoneAirMassFlowConservation,No;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is MinNumberOfFieldsError));
        }

        [Test]
        public void TestZoneAndSpaceSharingNameIsNotADuplicate()
        {
            // EnergyPlus allows a Space to share its Zone's name even though both
            // contribute to combined reference lists like
            // ZoneAndZoneListAndSpaceAndSpaceListNames.
            string idf = @"Version,25.2;
Zone,Zone A;
Space,Zone A,Zone A;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is DuplicateNameInReferenceListError));
        }

        [Test]
        public void TestZoneAndZoneListSharingNameIsADuplicate()
        {
            // Unlike Zone/Space, EnergyPlus rejects a ZoneList named after a Zone.
            string idf = @"Version,25.2;
Zone,Zone A;
ZoneList,Zone A,Zone A;";
            AssertError(idf, typeof(DuplicateNameInReferenceListError));
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
            var idfErrors = linter.Lint().Where(error => error is NumericFieldOutOfRangeError).ToList();
            Assert.IsTrue(!idfErrors.Any());
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
            var idfErrors = linter.Lint().Where(error => error is NumericFieldOutOfRangeError).ToList();
            Assert.IsTrue(idfErrors.Count == 0);
        }

        [Test]
        public void TestExclusiveMaximum()
        {
            string idf = "ZoneControl:Thermostat:OperativeTemperature,Name,Constant,0.9;";
            AssertError(idf, typeof(NumericFieldOutOfRangeError));
        }

        [Test]
        public void TestMissingDesignDaysAndRunPeriodsError()
        {
            string idf = "Version,25.2;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsTrue(errors.Any(error => error is MissingDesignDaysAndRunPeriodsError));
        }

        [Test]
        public void TestNoMissingEnvironmentErrorWithRunPeriod()
        {
            string idf = @"Version,25.2;
RunPeriod,
  Annual Run,
  1,1,12,31,
  Sunday,
  No,
  Yes,
  No,
  Yes;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is MissingDesignDaysAndRunPeriodsError));
        }

        [Test]
        public void TestNoMissingEnvironmentErrorWithDesignDay()
        {
            string idf = @"Version,25.2;
SizingPeriod:DesignDay,
  Example Winter,
  1,
  21,
  .4,
  4.5,
  ,
  -20,
  99000,
  3,
  330,
  Wetbulb,
  1,
  No,
  No,
  No,
  No;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is MissingDesignDaysAndRunPeriodsError));
        }


        [Test]
        public void TestMissingTimestepError()
        {
            string idf = "Version,25.2;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsTrue(errors.Any(error => error is MissingTimestepError));
        }

        [Test]
        public void TestNoMissingTimestepErrorWhenTimestepPresent()
        {
            string idf = "Version,25.2;\nTimestep,4;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is MissingTimestepError));
        }

        [Test]
        public void TestHeatingLoadSchemeOnCoolingLoop()
        {
            string idf = @"Version,25.2;
Sizing:Plant,CHW Loop,Cooling,6.7,5;
PlantLoop,CHW Loop,Water,,CHW Ops,CHW Outlet,98,1;
PlantEquipmentOperationSchemes,CHW Ops,PlantEquipmentOperation:HeatingLoad,Heating Scheme,Always On;";
            AssertError(idf, typeof(OperationSchemeLoopTypeMismatchError));
        }

        [Test]
        public void TestCoolingLoadSchemeOnHeatingLoop()
        {
            string idf = @"Version,25.2;
Sizing:Plant,HW Loop,Heating,82,11;
PlantLoop,HW Loop,Water,,HW Ops,HW Outlet,100,10;
PlantEquipmentOperationSchemes,HW Ops,PlantEquipmentOperation:CoolingLoad,Cooling Scheme,Always On;";
            AssertError(idf, typeof(OperationSchemeLoopTypeMismatchError));
        }

        [Test]
        public void TestHeatingLoadSchemeOnCondenserLoop()
        {
            string idf = @"Version,25.2;
Sizing:Plant,CW Loop,Condenser,29,5;
CondenserLoop,CW Loop,Water,,CW Ops,CW Outlet,80,10;
CondenserEquipmentOperationSchemes,CW Ops,PlantEquipmentOperation:HeatingLoad,Heating Scheme,Always On;";
            AssertError(idf, typeof(OperationSchemeLoopTypeMismatchError));
        }

        [Test]
        public void TestMatchingLoadSchemeOnCoolingLoopNoError()
        {
            string idf = @"Version,25.2;
Sizing:Plant,CHW Loop,Cooling,6.7,5;
PlantLoop,CHW Loop,Water,,CHW Ops,CHW Outlet,98,1;
PlantEquipmentOperationSchemes,CHW Ops,PlantEquipmentOperation:CoolingLoad,Cooling Scheme,Always On;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is OperationSchemeLoopTypeMismatchError));
        }

        [Test]
        public void TestLoopWithoutSizingPlantSchemeNotChecked()
        {
            // Without a Sizing:Plant object the loop's intent is unknown, so no
            // scheme type mismatch can be reported.
            string idf = @"Version,25.2;
PlantLoop,CHW Loop,Water,,CHW Ops,CHW Outlet,98,1;
PlantEquipmentOperationSchemes,CHW Ops,PlantEquipmentOperation:HeatingLoad,Heating Scheme,Always On;";
            IdfLinter linter = new IdfLinter(idf);
            var errors = linter.Lint();
            Assert.IsFalse(errors.Any(error => error is OperationSchemeLoopTypeMismatchError));
        }

        [Test]
        public void TestBuildingReferenceList()
        {
            string idf = "Schedule:Constant,  Test Schedule  ,,5;";

            ParsedIdf parsed = idf.ParseIdf();

            IdfLinter linter = new IdfLinter(idf);

            var result = linter.GetReferenceLists(parsed, IdfLinter.GroupByType(parsed));

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
