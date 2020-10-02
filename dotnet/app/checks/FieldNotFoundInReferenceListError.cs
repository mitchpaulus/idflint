using System.Collections.Generic;
using Antlr4.Runtime;

namespace dotnet.checks
{
    public class FieldNotFoundInReferenceListError : IdfError
    {
        private readonly IToken _token;
        private readonly string _fieldFound;
        private readonly List<string> _referenceListName;
        public int Id() => 6;
        public int Line() => _token.Line;

        public int Character() => _token.Column;

        public string Message() => $"The field {_fieldFound} was not found as a name in the {_referenceListName.OrList()} list.";

        public FieldNotFoundInReferenceListError(IToken token, string fieldFound, List<string> referenceListName)
        {
            _token = token;
            _fieldFound = fieldFound;
            _referenceListName = referenceListName;
        }
    }
}