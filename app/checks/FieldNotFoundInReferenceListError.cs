using System.Collections.Generic;

namespace dotnet.checks
{
    public class FieldNotFoundInReferenceListError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _fieldFound;
        private readonly List<string> _referenceListName;
        public int Id() => 6;
        public int Line() => _position.Line;

        public int Character() => _position.Column;

        public string Message() => $"The field {_fieldFound} was not found as a name in the {_referenceListName.OrList()} list.";

        public FieldNotFoundInReferenceListError(SourcePosition position, string fieldFound, List<string> referenceListName)
        {
            _position = position;
            _fieldFound = fieldFound;
            _referenceListName = referenceListName;
        }
    }
}