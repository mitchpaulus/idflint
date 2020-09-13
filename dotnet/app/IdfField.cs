using System.Collections.Generic;

namespace dotnet
{
    public class IdfField
    {
        public bool Required { get; set; } = false;
        public string Units { get; set; }
        public double Minimum { get; set; }
        public double Maximum { get; set; }
        public string Default { get; set; }
        public bool AutoSizeable { get; set; } = false;
        public List<string> Keys { get; set; } = new List<string>();

        public IdfField(bool required, string units, double minimum, double maximum, string defaultValue, bool autoSizeable)
        {
            Required = required;
            Units = units;
            Minimum = minimum;
            Maximum = maximum;
            Default = defaultValue;
            AutoSizeable = autoSizeable;
        }
    }

    public class IdfObject
    {
        public bool Unique { get; set; } = false;
        public IdfObjectFormat Format { get; set; }
        public bool Obsolete { get; set; } = false;
        public int MinNumberOfFields { get; set; }
        public bool Required { get; set; } = false;

        public IdfObject(bool unique, IdfObjectFormat format, bool obsolete, int minNumberOfFields, bool required)
        {
            Unique = unique;
            Format = format;
            Obsolete = obsolete;
            MinNumberOfFields = minNumberOfFields;
            Required = required;
        }
    }

    public class IdfUnit
    {
        
        
    }

    public enum IdfObjectFormat
    {
        SingleLine = 0,
        Vertices = 1,
        CompactSchedule = 2,
        FluidProperties = 3,
        ViewFactors = 4,
        Spectral = 5,
    }

    public enum IdfFieldType
    {
        Integer = 0,
        Real = 1,
        Alpha = 2,
        Choice = 3,
        ObjectList = 4,
        ExternalList = 5,
        Node = 6,
    }
    
    
    
    
}