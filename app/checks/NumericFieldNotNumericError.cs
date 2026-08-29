
namespace dotnet.checks
{
    public class NumericFieldNotNumericError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _fieldName;
        private readonly string _text;
        public int Id() => 5;

        public int Line() => _position.Line;

        public int Character() => _position.Column;

        public string Message() => $"The field '{_fieldName}' is expected to be numeric. Found '{_text}'.";

        public NumericFieldNotNumericError(SourcePosition position, string fieldName, string text)
        {
            _position = position;
            _fieldName = fieldName;
            _text = text;
        }
    }
}