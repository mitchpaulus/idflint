using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Idf;

namespace dotnet
{
    class Program
    {
        static int Main(string[] args)
        {
            if (args.Any(arg => arg == "-h" || arg == "--help"))
            {
                Console.WriteLine("idf-lint\n\nUSAGE:\nidf-lint idf_file\n\nidf-lint lints your idf-file for great good.\n");
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


    public class IdfLinter
    {
        private readonly TextReader _reader;

        public IdfLinter(TextReader reader) => _reader = reader;

        public IdfLinter(string idf) => _reader = new StringReader(idf);

        public List<IdfError> Lint()
        {
            List<IdfError> errors = new List<IdfError>();

            AntlrInputStream input = new AntlrInputStream(_reader);

            IdfErrorListener idfParseErrorListener = new IdfErrorListener();

            IdfLexer lexer = new IdfLexer(input);

            lexer.RemoveErrorListeners();
            IdfLexerErrorListener idfLexerErrorListener = new IdfLexerErrorListener();
            lexer.AddErrorListener(idfLexerErrorListener);

            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IdfParser parser = new IdfParser(tokens);

            parser.RemoveErrorListeners();
            parser.AddErrorListener(idfParseErrorListener);

            IdfParser.IdfContext tree = parser.idf();

            errors.AddRange(idfLexerErrorListener.Errors);
            errors.AddRange(idfParseErrorListener.Errors);

            ParseTreeWalker walker = new ParseTreeWalker();
            IdfLintListener idfLintListener = new IdfLintListener();
            walker.Walk(idfLintListener, tree);

            errors.AddRange(idfLintListener.errors);

            return errors;
        }
    }
}
