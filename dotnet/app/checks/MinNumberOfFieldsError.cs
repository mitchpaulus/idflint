using Antlr4.Runtime;

namespace dotnet.checks
{
    public class MinNumberOfFieldsError : IdfError
    {
        private readonly string _objectName;
        private readonly int _numberOfFieldsExpected;
        private readonly int _numberOfFieldsFound;
        private readonly int _line;
        private readonly int _col;
        public int Id() => 1;
        public int Line() => _line;

        public int Character() => _col;
        public string Message() => $"The object '{_objectName}' requires {_numberOfFieldsExpected} fields, {_numberOfFieldsFound} were supplied.";

        public MinNumberOfFieldsError(IToken startToken, string objectName, int numberOfFieldsExpected, int numberOfFieldsFound)
        {
            _objectName = objectName;
            _numberOfFieldsExpected = numberOfFieldsExpected;
            _numberOfFieldsFound = numberOfFieldsFound;
            _line = startToken.Line;
            _col = startToken.Column;
        }
    }
}