using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;

namespace dotnet
{
    public static class Extensions
    {
        public static void AddSafe<T1, T2>(this Dictionary<T1, List<T2>> dictionary, T1 key, T2 value)
        {
            if (!dictionary.ContainsKey(key)) dictionary[key] = new List<T2>();
            dictionary[key].Add(value);
        }

        public static IdfParser.IdfContext ParseIdf(this string input)
        {
            AntlrInputStream inputStream = new AntlrInputStream(input);
            
            IdfLexer lexer = new IdfLexer(inputStream);
            
            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IdfParser parser = new IdfParser(tokens);

            return parser.idf();
        }
    }
}