using System.Globalization;

namespace dotnet.checks
{
    /// <summary>
    /// A coefficient of performance field (units W/W) holding a value far above
    /// anything physically plausible, most often because the compressor input
    /// power in watts was entered instead of the capacity/input ratio.
    /// </summary>
    public class ImplausibleCopError : IdfError
    {
        /// <summary>COP values above this are reported.</summary>
        public const double Threshold = 20.0;

        private readonly SourcePosition _position;
        private readonly string _objectType;
        private readonly string _fieldName;
        private readonly double _cop;
        private readonly double? _capacity;

        public int Id() => 15;
        public int Line() => _position.Line;
        public int Character() => _position.Column;

        public string Message()
        {
            string cop = _cop.ToString("G", CultureInfo.InvariantCulture);
            string expectation = "This field expects capacity divided by input power, not the input power in W.";

            if (_capacity.HasValue)
            {
                string capacity = _capacity.Value.ToString("G", CultureInfo.InvariantCulture);
                string impliedPower = (_capacity.Value / _cop).ToString("0.###", CultureInfo.InvariantCulture);
                return $"{_objectType} field {_fieldName} is {cop} W/W, implying only {impliedPower} W of input power for {capacity} W of capacity. {expectation}";
            }

            return $"{_objectType} field {_fieldName} is {cop} W/W, above the plausible limit of {Threshold}. {expectation}";
        }

        public ImplausibleCopError(SourcePosition position, string objectType, string fieldName, double cop, double? capacity)
        {
            _position = position;
            _objectType = objectType;
            _fieldName = fieldName;
            _cop = cop;
            _capacity = capacity;
        }
    }
}
