using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

namespace dotnet
{
    public class IdfErrorListener : BaseErrorListener
    {
        // public bool ParseError = false;
        public List<IdfParseError> Errors = new List<IdfParseError>();
        
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine,
            string msg, RecognitionException e)
        {
           // base.SyntaxError(output, recognizer, offendingSymbol, line, charPositionInLine, msg, e);
           Errors.Add(new IdfParseError(line, charPositionInLine, msg));
        }
    }

    public class IdfParseError
    {
        public int Line;
        public int Character;
        public string Message;

        public IdfParseError(int line, int character, string message)
        {
            Line = line;
            Character = character;
            Message = message;
        }

        public string ErrorText() => $"{Line}:{Character}: {Message}";
    }
}