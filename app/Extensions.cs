using System.Collections.Generic;
using System.Linq;

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

        public static ParsedIdf ParseIdf(this string input) => IdfSourceParser.Parse(input);

        public static string JoinStrings(this IEnumerable<string> strings) => string.Join(",", strings.Select(s => $"\"{s}\""));
    }
}
