using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using dotnet.checks;
using Idf;

namespace dotnet
{
    public class IdfLinter
    {
        private readonly TextReader _reader;
        private IdfObjectProvider _provider;

        public IdfLinter(TextReader reader) => _reader = reader;

        public IdfLinter(string idf) => _reader = new StringReader(idf);

        public IdfLinter(string idf, IdfObjectProvider provider)
        {
            _reader = new StringReader(idf);
            _provider = provider;
        }

        /// <summary>
        /// Opens the object database matching the file's Version object (downloading
        /// it on first use). No-op if a provider was already supplied or resolved.
        /// </summary>
        private void EnsureProvider(Dictionary<string, List<IdfParser.ObjectContext>> idfObjects)
        {
            if (_provider != null) return;

            string versionText = null;
            if (idfObjects.TryGetValue("Version", out var versionContexts) && versionContexts.Count > 0)
            {
                var versionFields = versionContexts[0].fields().field();
                if (versionFields.Length > 0) versionText = versionFields[0].GetText().Trim();
            }

            _provider = IdfObjectProvider.ForVersion(versionText);
        }

        public List<IdfError> Lint()
        {
            List<IdfError> errors = new List<IdfError>();

            AntlrInputStream input = new AntlrInputStream(_reader);

            IdfErrorListener idfParseErrorListener = new IdfErrorListener();

            IdfLexer lexer = new IdfLexer(input);

            lexer.RemoveErrorListeners();
            IdfLexerErrorListener idfLexerErrorListener = new IdfLexerErrorListener();
            lexer.AddErrorListener(idfLexerErrorListener);

            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IdfParser parser = new IdfParser(tokens);

            parser.RemoveErrorListeners();
            parser.AddErrorListener(idfParseErrorListener);

            IdfParser.IdfContext tree = parser.idf();

            errors.AddRange(idfLexerErrorListener.Errors);
            errors.AddRange(idfParseErrorListener.Errors);

            ParseTreeWalker walker = new ParseTreeWalker();
            IdfLintListener idfLintListener = new IdfLintListener();
            walker.Walk(idfLintListener, tree);

            var inputData = idfLintListener.IdfObjects;

            errors.AddRange(idfLintListener.errors);

            EnsureProvider(inputData);

            foreach (var unknownTypeName in inputData.Keys.Where(key => !_provider.ContainsKey(key)).ToList())
            {
                foreach (var objectContext in inputData[unknownTypeName])
                {
                    errors.Add(new ObjectTypeNotFoundError(objectContext.ALPHA().Symbol, unknownTypeName));
                }
                // Don't check any of the fields if we don't know what the object is.
                inputData.Remove(unknownTypeName);
            }

            foreach (var inputDataKey in inputData.Keys)
            {
                IdfObject idfObject = _provider.GetIdfObject(inputDataKey);
                foreach (var objectContext in inputData[inputDataKey])
                {
                    errors.AddRange(idfObject.FieldChecks(objectContext));
                }
            }

            ReferenceListResult referenceListResult = GetReferenceLists(idfLintListener.IdfObjects);

            errors.AddRange(referenceListResult.Errors);

            foreach (var boundField in inputData.BoundFields(_provider).Where(field => field.ExpectedField.ObjectList.Any()))
            {
                // It's not an error if the field is empty and not required.
                if (string.IsNullOrWhiteSpace(boundField.FoundField) && !boundField.ExpectedField.Required) continue;

                Dictionary<string, HashSet<string>> referenceList = referenceListResult.ReferenceList;

                var inRegularReferenceList = boundField.ExpectedField.ObjectList.Any(objectListType => InReferenceList(referenceList, objectListType, boundField));
                var inReferenceClassList = boundField.ExpectedField.ObjectList.Any(objectListType => InReferenceClassList(objectListType, boundField.FoundField));
                if (!inRegularReferenceList && !inReferenceClassList)
                {
                    errors.Add(new FieldNotFoundInReferenceListError(boundField.FieldContext.Start, boundField.FoundField, boundField.ExpectedField.ObjectList));
                }
            }

            foreach (var requiredObjectName in _provider.RequiredObjectNames)
            {
                if (!inputData.TryGetValue(requiredObjectName, out var objectInstances) || objectInstances == null || objectInstances.Count == 0)
                {
                    errors.Add(new RequiredObjectTypeNotFoundError(requiredObjectName));
                }
            }

            string[] designDayObjectNames =
            {
                "SizingPeriod:DesignDay",
                "SizingPeriod:WeatherFileDays",
                "SizingPeriod:WeatherFileConditionType"
            };

            bool hasDesignDay = designDayObjectNames.Any(name =>
                inputData.TryGetValue(name, out var contexts) && contexts != null && contexts.Count > 0);

            bool hasRunPeriod = inputData.TryGetValue("RunPeriod", out var runPeriodContexts) &&
                                runPeriodContexts != null && runPeriodContexts.Count > 0;

            if (!hasDesignDay && !hasRunPeriod)
            {
                errors.Add(new MissingDesignDaysAndRunPeriodsError());
            }

            return errors;
        }

        private bool InReferenceList(Dictionary<string, HashSet<string>> referenceList, string objectListType, BoundField boundField)
        {
            return referenceList.ContainsKey(objectListType) && referenceList[objectListType].Contains(boundField.FoundField);
        }

        public bool InReferenceClassList(string objectListType, string foundField) =>
            _provider.ReferenceClassList.ContainsKey(objectListType) &&
            _provider.ReferenceClassList[objectListType].Contains(foundField);

        /// <summary>
        /// Build up a Dictionary data structure for reference lists.
        /// Key: Reference List name from the IDD. Example: 'ScheduleNames'
        /// Value: List of possible names for that reference.
        /// </summary>
        /// <param name="data"></param>
        /// <returns></returns>
        public ReferenceListResult GetReferenceLists(Dictionary<string, List<IdfParser.ObjectContext>> data)
        {
            Dictionary<string, HashSet<string>> referenceListDictionary = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
            List<IdfError> errors = new List<IdfError>();

            EnsureProvider(data);

            foreach (string key in data.Keys)
            {
                if (!_provider.ContainsKey(key)) continue;
                IdfObject idfObject = _provider.GetIdfObject(key);

                foreach (var objectContext in data[key])
                {
                    var fields = objectContext.fields().field();
                    var boundFields = idfObject.ZipWithFields(fields);

                    foreach (var boundField in boundFields)
                    {
                        // Add the field text to the reference list. See \reference in the IDD.
                        foreach (var refList in boundField.ExpectedField.ReferenceList)
                        {
                            if (!referenceListDictionary.ContainsKey(refList)) referenceListDictionary[refList] = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                            bool addedSuccessfully = referenceListDictionary[refList].Add(boundField.FoundField.Trim());
                            if (!addedSuccessfully)
                            {
                                errors.Add(new DuplicateNameInReferenceListError(boundField.FieldContext.Start, boundField.FoundField, refList));
                            }
                        }
                    }
                }
            }

            AddDefaultSpaces(data, referenceListDictionary);

            return new ReferenceListResult(referenceListDictionary, errors);
        }

        private void AddDefaultSpaces(Dictionary<string, List<IdfParser.ObjectContext>> data, Dictionary<string, HashSet<string>> referenceListDictionary)
        {
            if (!data.TryGetValue("Zone", out var zoneContexts) || zoneContexts.Count == 0) return;

            var zoneObject = _provider.GetIdfObject("Zone");
            HashSet<string> zoneNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

            foreach (var zoneContext in zoneContexts)
            {
                if (zoneObject.TryGetFieldValue(zoneContext, "Name", out var zoneName) &&
                    !string.IsNullOrWhiteSpace(zoneName))
                {
                    zoneNames.Add(zoneName);
                }
            }

            if (zoneNames.Count == 0) return;

            HashSet<string> zonesWithSpaces = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            if (data.TryGetValue("Space", out var spaceContexts) && spaceContexts.Count > 0)
            {
                var spaceObject = _provider.GetIdfObject("Space");
                foreach (var spaceContext in spaceContexts)
                {
                    if (spaceObject.TryGetFieldValue(spaceContext, "Zone Name", out var zoneName) &&
                        !string.IsNullOrWhiteSpace(zoneName))
                    {
                        zonesWithSpaces.Add(zoneName);
                    }
                }
            }

            if (!referenceListDictionary.TryGetValue("SpaceNames", out var spaceNames))
            {
                spaceNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                referenceListDictionary["SpaceNames"] = spaceNames;
            }

            foreach (var zoneName in zoneNames)
            {
                if (!zonesWithSpaces.Contains(zoneName))
                {
                    spaceNames.Add(zoneName);
                }
            }
        }

    }

    public class ReferenceListResult
    {
        public Dictionary<string, HashSet<string>> ReferenceList;
        public List<IdfError> Errors;

        public ReferenceListResult(Dictionary<string, HashSet<string>> referenceList, List<IdfError> errors)
        {
            ReferenceList = referenceList;
            Errors = errors;
        }
    }
}
