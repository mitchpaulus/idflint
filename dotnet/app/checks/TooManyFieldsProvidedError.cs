using Antlr4.Runtime;

namespace dotnet.checks
{
    public class TooManyFieldsProvidedError : IdfError
    {
        private readonly IToken _token;
        private readonly string _objectType;
        private readonly int _expectedNumberOfFields;
        private readonly int _actualNumberOfFields;
        public int Id() => 4;

        public int Line() => _token.Line;
        public int Character() => _token.Column;
        public string Message() => $"The object type '{_objectType}' only has {_expectedNumberOfFields} fields defined. {_actualNumberOfFields} were provided.";

        public TooManyFieldsProvidedError(IToken token, string objectType, int expectedNumberOfFields, int actualNumberOfFields)
        {
            _token = token;
            _objectType = objectType;
            _expectedNumberOfFields = expectedNumberOfFields;
            _actualNumberOfFields = actualNumberOfFields;
        }
    }
}