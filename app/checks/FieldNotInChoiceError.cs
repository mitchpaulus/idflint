using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;

namespace dotnet.checks
{
    public class FieldNotInChoiceError : IdfError
    {
        private readonly IToken _token;
        private readonly string _fieldName;
        private readonly IEnumerable<string> _availableChoices;
        private readonly string _actualChoice;
        public int Id() => 2;

        public int Line() => _token.Line;
        public int Character() => _token.Column;
        public string Message()
        {
            return _availableChoices.Count() > 2 ?
                $"The field '{_fieldName}' is expected to be one of {_availableChoices.OrList()}. Saw '{_actualChoice}'." :
                $"The field '{_fieldName}' is expected to be {_availableChoices.OrList()}. Saw '{_actualChoice}'.";
        }

        public FieldNotInChoiceError(IToken token, string fieldName, IEnumerable<string> availableChoices, string actualChoice)
        {
            _token = token;
            _fieldName = fieldName;
            _availableChoices = availableChoices;
            _actualChoice = actualChoice;
        }
    }
}