using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

namespace dotnet
{
    class Program
    {
        static int Main(string[] args)
        {
            if (args.Any(arg => arg == "-h" || arg == "--help"))
            {
                Console.WriteLine("idflint\n\nUSAGE:\nidflint idf_file\n\nidflint lints your idf file for great good.\n");
                return 0;
            }

            TextReader reader = args.Any() ? new StreamReader(args[0]) : Console.In;

            IdfLinter linter = new IdfLinter(reader);
            var errors = linter.Lint();
            errors.WriteErrors();

            return errors.Any() ? 1 : 0;
        }
    }

    public class IdfLexerErrorListener : IAntlrErrorListener<int>
    {
        public readonly List<IdfParseError> Errors = new List<IdfParseError>();

        public void SyntaxError(TextWriter output, IRecognizer recognizer, int offendingSymbol, int line, int charPositionInLine,
            string msg, RecognitionException e)
        {
            Errors.Add(new IdfParseError(line, charPositionInLine, msg));
        }
    }
}
