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
        static void Main(string[] args)
        {
            TextReader reader = args.Any() ? new StreamReader(args[0]) : Console.In;
            
            AntlrInputStream input = new AntlrInputStream(reader);

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

            if (idfParseErrorListener.Errors.Any() || idfLexerErrorListener.Errors.Any())
            {
                foreach (IdfParseError idfParseError in idfLexerErrorListener.Errors.Concat(idfParseErrorListener.Errors))
                    Console.WriteLine(idfParseError.ErrorText());
                return;
            }
            
            Console.WriteLine(tree.ToStringTree(parser));

            ParseTreeWalker walker = new ParseTreeWalker();

            walker.Walk(new EplusListener(), tree);
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
