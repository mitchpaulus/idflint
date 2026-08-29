namespace dotnet.checks
{
    public class OperationSchemeLoopTypeMismatchError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _loopName;
        private readonly string _loopType;
        private readonly string _schemeType;

        public int Id() => 13;
        public int Line() => _position.Line;
        public int Character() => _position.Column;

        public string Message() =>
            $"Loop '{_loopName}' is a {_loopType} loop per its Sizing:Plant object, but its operation schemes use {_schemeType}.";

        public OperationSchemeLoopTypeMismatchError(SourcePosition position, string loopName, string loopType, string schemeType)
        {
            _position = position;
            _loopName = loopName;
            _loopType = loopType;
            _schemeType = schemeType;
        }
    }
}
