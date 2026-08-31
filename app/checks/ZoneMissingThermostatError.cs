namespace dotnet.checks
{
    public class ZoneMissingThermostatError : IdfError
    {
        private readonly SourcePosition _position;
        private readonly string _zoneName;

        public int Id() => 14;
        public int Line() => _position.Line;
        public int Character() => _position.Column;

        public string Message() =>
            $"Zone '{_zoneName}' has HVAC equipment connections but no thermostat (ZoneControl:Thermostat) associated with it.";

        public ZoneMissingThermostatError(SourcePosition position, string zoneName)
        {
            _position = position;
            _zoneName = zoneName;
        }
    }
}
