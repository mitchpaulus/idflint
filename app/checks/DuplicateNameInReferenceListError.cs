using Antlr4.Runtime;

namespace dotnet.checks
{
    public class DuplicateNameInReferenceListError : IdfError
    {
        private readonly IToken _token;
        private readonly string _name;
        private readonly string _referenceList;
        public int Id() => 7;

        public int Line() => _token.Line;
        public int Character() => _token.Column;
        public string Message() => $"The name '{_name}' was already present in the '{_referenceList}' list.";

        public DuplicateNameInReferenceListError(IToken token, string name, string referenceList)
        {
            _token = token;
            _name = name;
            _referenceList = referenceList;
        }
    }
}