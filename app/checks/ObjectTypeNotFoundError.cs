using Antlr4.Runtime;

namespace dotnet.checks
{
    public class ObjectTypeNotFoundError : IdfError
    {
        private readonly IToken _token;
        private readonly string _enteredObjectType;
        public int Id() => 3;

        public int Line() => _token.Line;

        public int Character() => _token.Column;
        public string Message() => $"{_enteredObjectType} is not a known object type.";

        public ObjectTypeNotFoundError(IToken token, string enteredObjectType)
        {
            _token = token;
            _enteredObjectType = enteredObjectType;
        }
    }
}