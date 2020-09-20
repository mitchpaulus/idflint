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

    public interface IdfError
    {
        public int Id();
        public int Line();
        public int Character();
        public string Message();
    }

    public class IdfParseError : IdfError
    {
        private int _line;
        public int Char;
        public string Text;

        public IdfParseError(int line, int character, string message)
        {
            _line = line;
            Char = character;
            Text = message;
        }

        public string ErrorText() => Text;
        public int Id() => 2;
        public int Line() => _line;

        public int Character() => Char;
        public string Message() => ErrorText();
    }
}