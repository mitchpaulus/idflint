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

        public IdfLinter(TextReader reader) => _reader = reader;

        public IdfLinter(string idf) => _reader = new StringReader(idf);

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

            foreach (var inputDataKey in inputData.Keys)
            {
                IdfObject idfObject = IdfObjectList.Objects[inputDataKey];
                foreach (var objectContext in inputData[inputDataKey])
                {
                    errors.AddRange(idfObject.FieldChecks(objectContext));
                }
            }

            var referenceList = GetReferenceLists(idfLintListener.IdfObjects);

            errors.AddRange(referenceList.Errors);

            foreach (var boundField in inputData.BoundFields())
            {
                foreach (string objectList in boundField.ExpectedField.ObjectList)
                {
                    // It's not an error if the field is empty and not required.
                    if (string.IsNullOrWhiteSpace(boundField.FoundField)) continue;

                    if (!referenceList.ReferenceList.ContainsKey(objectList) ||
                        !referenceList.ReferenceList[objectList].Contains(boundField.FoundField))
                    {
                        errors.Add(new FieldNotFoundInReferenceListError(boundField.FieldContext.Start, boundField.FoundField, objectList));
                    }
                }
            }

            return errors;
        }

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

            foreach (string key in data.Keys)
            {
                IdfObject idfObject = IdfObjectList.Objects[key];

                foreach (var objectContext in data[key])
                {
                    var fields = objectContext.fields().field();
                    var boundFields = idfObject.ZipWithFields(fields);

                    foreach (var boundField in boundFields)
                    {
                        foreach (var refList in boundField.ExpectedField.ReferenceList)
                        {
                            if (!referenceListDictionary.ContainsKey(refList)) referenceListDictionary[refList] = new HashSet<string>();
                            bool addedSuccessfully = referenceListDictionary[refList].Add(boundField.FoundField);
                            if (!addedSuccessfully)
                            {
                                errors.Add(new DuplicateNameInReferenceListError(boundField.FieldContext.Start, boundField.FoundField, refList));
                            }
                        }
                    }
                }
            }

            return new ReferenceListResult(referenceListDictionary, errors);
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