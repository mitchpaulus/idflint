
namespace dotnet.checks
{

    public class MinMax
    {
        public static MinMax Minimum = new MinMax("minimum");
        public static MinMax Maximum = new MinMax("maximum");

        public readonly string Description;
        private MinMax(string description)
        {
            Description = description;
        }
    }

    public class NumericFieldOutOfRangeError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly MinMax _minMax;
        private readonly IdfFieldMinMaxType _minMaxType;
        private readonly string _valueEntered;
        private readonly double _boundsValue;
        private readonly string _fieldName;
        public int Id() => 8;
        public int Line() => _position.Line;
        public int Character() => _position.Column;
        public string Message() => $"The field {_fieldName} has a {_minMax.Description} of {_boundsValue} ({_minMaxType}), {_valueEntered} was entered.";

        public NumericFieldOutOfRangeError(SourcePosition position, MinMax minMax, IdfFieldMinMaxType minMaxType, string valueEntered, double boundsValue, string fieldName)
        {
            _position = position;
            _minMax = minMax;
            _minMaxType = minMaxType;
            _valueEntered = valueEntered;
            _boundsValue = boundsValue;
            _fieldName = fieldName;
        }
    }
}