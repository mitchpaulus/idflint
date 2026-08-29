
namespace dotnet.checks
{
    public class DuplicateNameInReferenceListError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _name;
        private readonly string _referenceList;
        public int Id() => 7;

        public int Line() => _position.Line;
        public int Character() => _position.Column;
        public string Message() => $"The name '{_name}' was already present in the '{_referenceList}' list.";

        public DuplicateNameInReferenceListError(SourcePosition position, string name, string referenceList)
        {
            _position = position;
            _name = name;
            _referenceList = referenceList;
        }
    }
}