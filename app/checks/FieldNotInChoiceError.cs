using System.Collections.Generic;
using System.Linq;

namespace dotnet.checks
{
    public class FieldNotInChoiceError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _fieldName;
        private readonly IEnumerable<string> _availableChoices;
        private readonly string _actualChoice;
        public int Id() => 2;

        public int Line() => _position.Line;
        public int Character() => _position.Column;
        public string Message()
        {
            return _availableChoices.Count() > 2 ?
                $"The field '{_fieldName}' is expected to be one of {_availableChoices.OrList()}. Saw '{_actualChoice}'." :
                $"The field '{_fieldName}' is expected to be {_availableChoices.OrList()}. Saw '{_actualChoice}'.";
        }

        public FieldNotInChoiceError(SourcePosition position, string fieldName, IEnumerable<string> availableChoices, string actualChoice)
        {
            _position = position;
            _fieldName = fieldName;
            _availableChoices = availableChoices;
            _actualChoice = actualChoice;
        }
    }
}