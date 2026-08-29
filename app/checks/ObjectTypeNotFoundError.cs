
namespace dotnet.checks
{
    public class ObjectTypeNotFoundError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _enteredObjectType;
        public int Id() => 3;

        public int Line() => _position.Line;

        public int Character() => _position.Column;
        public string Message() => $"{_enteredObjectType} is not a known object type.";

        public ObjectTypeNotFoundError(SourcePosition position, string enteredObjectType)
        {
            _position = position;
            _enteredObjectType = enteredObjectType;
        }
    }
}