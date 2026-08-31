namespace dotnet.checks
{
    public class PlantLoopTemperatureLimitsError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _objectType;
        private readonly string _loopName;
        private readonly double _minimum;
        private readonly double _maximum;

        public int Id() => 12;
        public int Line() => _position.Line;
        public int Character() => _position.Column;

        public string Message() =>
            $"{_objectType} '{_loopName}' has a Minimum Loop Temperature ({_minimum}) greater than its Maximum Loop Temperature ({_maximum}).";

        public PlantLoopTemperatureLimitsError(SourcePosition position, string objectType, string loopName, double minimum, double maximum)
        {
            _position = position;
            _objectType = objectType;
            _loopName = loopName;
            _minimum = minimum;
            _maximum = maximum;
        }
    }
}
