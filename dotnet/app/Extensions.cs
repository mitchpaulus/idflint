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

        public static void AddSafe<T1, T2>(this Dictionary<T1, List<T2>> dictionary, T1 key, IEnumerable<T2> values)
        {
            if (!dictionary.ContainsKey(key)) dictionary[key] = new List<T2>();
            dictionary[key].AddRange(values);
        }

        public static IdfParser.IdfContext ParseIdf(this string input)
        {
            AntlrInputStream inputStream = new AntlrInputStream(input);

            IdfLexer lexer = new IdfLexer(inputStream);

            CommonTokenStream tokens = new CommonTokenStream(lexer);

            IdfParser parser = new IdfParser(tokens);

            return parser.idf();
        }

        public static List<IdfParser.ObjectContext> AllObjects(
            this Dictionary<string, List<IdfParser.ObjectContext>> objectDictionary) =>
            objectDictionary.Keys.SelectMany(key => objectDictionary[key]).ToList();

        public static List<(IdfParser.ObjectContext objectContext, IdfObject idfObject)> BoundObjects(
            this Dictionary<string, List<IdfParser.ObjectContext>> data)
        {
            return data.SelectMany(pair =>
            {
                IdfObject idfObject = IdfObjectList.GetIdfObject(pair.Key);
                return pair.Value.Select(context => (context, idfObject));
            }).ToList();
        }

        public static List<BoundField> BoundFields(this Dictionary<string, List<IdfParser.ObjectContext>> data)
        {
            return data.BoundObjects().SelectMany(tuple => tuple.idfObject.ZipWithFields(tuple.objectContext.fields().field())).ToList();
        }

        public static string JoinStrings(this IEnumerable<string> strings) => string.Join(",", strings.Select(s => $"\"{s}\""));

    }

    public static class IdfObjectExtensions
    {
        public static List<string> Fields(this IdfParser.ObjectContext objectContext) => objectContext.fields().field().Select(context => context.GetText().Trim()).ToList();
    }
}