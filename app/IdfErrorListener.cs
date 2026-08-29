namespace dotnet
{
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