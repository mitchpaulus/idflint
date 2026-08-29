using dotnet;

namespace dotnet.checks
{
    public class MissingTimestepError : IdfError
    {
        public int Id() => 11;
        public int Line() => 0;
        public int Character() => 0;
        public string Message() =>
            "No 'Timestep' object found. Number of timesteps in hour will default to 4.";
    }
}
