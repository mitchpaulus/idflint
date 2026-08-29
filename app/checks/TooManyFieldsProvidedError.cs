
namespace dotnet.checks
{
    public class TooManyFieldsProvidedError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _objectType;
        private readonly int _expectedNumberOfFields;
        private readonly int _actualNumberOfFields;
        public int Id() => 4;

        public int Line() => _position.Line;
        public int Character() => _position.Column;
        public string Message() => $"The object type '{_objectType}' only has {_expectedNumberOfFields} fields defined. {_actualNumberOfFields} were provided.";

        public TooManyFieldsProvidedError(SourcePosition position, string objectType, int expectedNumberOfFields, int actualNumberOfFields)
        {
            _position = position;
            _objectType = objectType;
            _expectedNumberOfFields = expectedNumberOfFields;
            _actualNumberOfFields = actualNumberOfFields;
        }
    }
}