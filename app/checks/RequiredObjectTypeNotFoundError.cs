using dotnet;

namespace dotnet.checks
{
    public class RequiredObjectTypeNotFoundError : IdfError
    {
        private readonly string _objectType;
        public int Id() => 9;
        public int Line() => 0;
        public int Character() => 0;
        public string Message() => $"At least one '{_objectType}' object is required but none were found.";

        public RequiredObjectTypeNotFoundError(string objectType)
        {
            _objectType = objectType;
        }
    }
}
