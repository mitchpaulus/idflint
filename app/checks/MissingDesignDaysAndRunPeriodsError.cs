using dotnet;

namespace dotnet.checks
{
    public class MissingDesignDaysAndRunPeriodsError : IdfError
    {
        public int Id() => 10;
        public int Line() => 0;
        public int Character() => 0;
        public string Message() =>
            "No Design Days or Run Period(s) specified.";
    }
}
