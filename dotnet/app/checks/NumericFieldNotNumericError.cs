using Antlr4.Runtime;

namespace dotnet.checks
{
    public class NumericFieldNotNumericError : IdfError
    {
        private readonly IToken _token;
        private readonly string _fieldName;
        private readonly string _text;
        public int Id() => 5;

        public int Line() => _token.Line;

        public int Character() => _token.Column;

        public string Message() => $"The field '{_fieldName}' is expected to be numeric. Found '{_text}'.";

        public NumericFieldNotNumericError(IToken token, string fieldName, string text)
        {
            _token = token;
            _fieldName = fieldName;
            _text = text;
        }
    }
}