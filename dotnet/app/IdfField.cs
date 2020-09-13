using System;
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
        public IdfFieldAlphaNumeric AlphaNumeric { get; set; }

        public IdfField(bool required, string units, double minimum, double maximum, string defaultValue, bool autoSizeable, IdfFieldAlphaNumeric alphaNumeric)
        {
            Required = required;
            Units = units;
            Minimum = minimum;
            Maximum = maximum;
            Default = defaultValue;
            AutoSizeable = autoSizeable;
            AlphaNumeric = alphaNumeric;
        }
    }

    public class IdfObject
    {
        public bool Unique { get; set; } = false;
        public IdfObjectFormat Format { get; set; }
        public bool Obsolete { get; set; } = false;
        public int MinNumberOfFields { get; set; }
        public bool Required { get; set; } = false;
        
        public string Name { get; set; }

        public IdfObject(string name, bool unique, IdfObjectFormat format, bool obsolete, int minNumberOfFields,
            bool required)
        {
            Unique = unique;
            Format = format;
            Obsolete = obsolete;
            MinNumberOfFields = minNumberOfFields;
            Required = required;
            Name = name;
        }
    }

    public class IdfUnit
    {
        
        
    }

    public enum IdfFieldAlphaNumeric
    {
        Alpha = 0,
        Numeric = 1,
    }

    public enum IdfObjectFormat
    {
        NotSpecified = -1,
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

    public static class IdfObjectList
    {
        public static Dictionary<string, IdfObject> Objects = new Dictionary<string, IdfObject>(StringComparer.OrdinalIgnoreCase)
        {
            {"Version", new IdfObject("Version", true, IdfObjectFormat.NotSpecified, false, -1, false)},
            { "SimulationControl", new IdfObject("SimulationControl", true, IdfObjectFormat.NotSpecified, false, 5, false) },
            { "PerformancePrecisionTradeoffs", new IdfObject("PerformancePrecisionTradeoffs", true, IdfObjectFormat.NotSpecified, false, -1, false) },
            {"Building", new IdfObject("Building", true, IdfObjectFormat.NotSpecified, false, 8, true)},
            { "ShadowCalculation", new IdfObject("ShadowCalculation", true, IdfObjectFormat.NotSpecified, false, -1, false) },
            { "SurfaceConvectionAlgorithm:Inside", new IdfObject("SurfaceConvectionAlgorithm:Inside", true, IdfObjectFormat.NotSpecified, false, -1, false) }, 
            { "SurfaceConvectionAlgorithm:Outside", new IdfObject("SurfaceConvectionAlgorithm:Outside", true, IdfObjectFormat.NotSpecified, false, -1, false) },
            { "HeatBalanceAlgorithm", new IdfObject("HeatBalanceAlgorithm", true, IdfObjectFormat.NotSpecified, false, -1, false) },
            {
                "HeatBalanceSettings:ConductionFiniteDifference",
                new IdfObject("HeatBalanceSettings:ConductionFiniteDifference", true, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "ZoneAirHeatBalanceAlgorithm",
                new IdfObject("ZoneAirHeatBalanceAlgorithm", true, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {
                "ZoneAirContaminantBalance",
                new IdfObject("ZoneAirContaminantBalance", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneAirMassFlowConservation",
                new IdfObject("ZoneAirMassFlowConservation", true, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ZoneCapacitanceMultiplier:ResearchSpecial",
                new IdfObject("ZoneCapacitanceMultiplier:ResearchSpecial", false, IdfObjectFormat.NotSpecified, false,
                    6, false)
            },
            {"Timestep", new IdfObject("Timestep", true, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "ConvergenceLimits",
                new IdfObject("ConvergenceLimits", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "HVACSystemRootFindingAlgorithm",
                new IdfObject("HVACSystemRootFindingAlgorithm", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Compliance:Building",
                new IdfObject("Compliance:Building", true, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {"Site:Location", new IdfObject("Site:Location", true, IdfObjectFormat.NotSpecified, false, 5, false)},
            {
                "Site:VariableLocation",
                new IdfObject("Site:VariableLocation", true, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {
                "SizingPeriod:DesignDay",
                new IdfObject("SizingPeriod:DesignDay", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SizingPeriod:WeatherFileDays",
                new IdfObject("SizingPeriod:WeatherFileDays", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SizingPeriod:WeatherFileConditionType",
                new IdfObject("SizingPeriod:WeatherFileConditionType", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {"RunPeriod", new IdfObject("RunPeriod", false, IdfObjectFormat.NotSpecified, false, 7, false)},
            {
                "RunPeriodControl:SpecialDays",
                new IdfObject("RunPeriodControl:SpecialDays", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "RunPeriodControl:DaylightSavingTime",
                new IdfObject("RunPeriodControl:DaylightSavingTime", true, IdfObjectFormat.NotSpecified, false, 2,
                    false)
            },
            {
                "WeatherProperty:SkyTemperature",
                new IdfObject("WeatherProperty:SkyTemperature", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:WeatherStation",
                new IdfObject("Site:WeatherStation", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:HeightVariation",
                new IdfObject("Site:HeightVariation", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:GroundTemperature:BuildingSurface",
                new IdfObject("Site:GroundTemperature:BuildingSurface", true, IdfObjectFormat.NotSpecified, false, 12,
                    false)
            },
            {
                "Site:GroundTemperature:FCfactorMethod",
                new IdfObject("Site:GroundTemperature:FCfactorMethod", true, IdfObjectFormat.NotSpecified, false, 12,
                    false)
            },
            {
                "Site:GroundTemperature:Shallow",
                new IdfObject("Site:GroundTemperature:Shallow", true, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "Site:GroundTemperature:Deep",
                new IdfObject("Site:GroundTemperature:Deep", true, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "Site:GroundTemperature:Undisturbed:FiniteDifference",
                new IdfObject("Site:GroundTemperature:Undisturbed:FiniteDifference", false,
                    IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "Site:GroundTemperature:Undisturbed:KusudaAchenbach",
                new IdfObject("Site:GroundTemperature:Undisturbed:KusudaAchenbach", false, IdfObjectFormat.NotSpecified,
                    false, 7, false)
            },
            {
                "Site:GroundTemperature:Undisturbed:Xing",
                new IdfObject("Site:GroundTemperature:Undisturbed:Xing", false, IdfObjectFormat.NotSpecified, false, 9,
                    false)
            },
            {
                "Site:GroundDomain:Slab",
                new IdfObject("Site:GroundDomain:Slab", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:GroundDomain:Basement",
                new IdfObject("Site:GroundDomain:Basement", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:GroundReflectance",
                new IdfObject("Site:GroundReflectance", true, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "Site:GroundReflectance:SnowModifier",
                new IdfObject("Site:GroundReflectance:SnowModifier", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Site:WaterMainsTemperature",
                new IdfObject("Site:WaterMainsTemperature", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:Precipitation",
                new IdfObject("Site:Precipitation", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"RoofIrrigation", new IdfObject("RoofIrrigation", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Site:SolarAndVisibleSpectrum",
                new IdfObject("Site:SolarAndVisibleSpectrum", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Site:SpectrumData",
                new IdfObject("Site:SpectrumData", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "ScheduleTypeLimits",
                new IdfObject("ScheduleTypeLimits", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Schedule:Day:Hourly",
                new IdfObject("Schedule:Day:Hourly", false, IdfObjectFormat.NotSpecified, false, 26, false)
            },
            {
                "Schedule:Day:Interval",
                new IdfObject("Schedule:Day:Interval", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Schedule:Day:List",
                new IdfObject("Schedule:Day:List", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Schedule:Week:Daily",
                new IdfObject("Schedule:Week:Daily", false, IdfObjectFormat.NotSpecified, false, 13, false)
            },
            {
                "Schedule:Week:Compact",
                new IdfObject("Schedule:Week:Compact", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {"Schedule:Year", new IdfObject("Schedule:Year", false, IdfObjectFormat.NotSpecified, false, 7, false)},
            {
                "Schedule:Compact",
                new IdfObject("Schedule:Compact", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Schedule:Constant",
                new IdfObject("Schedule:Constant", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Schedule:File:Shading",
                new IdfObject("Schedule:File:Shading", true, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {"Schedule:File", new IdfObject("Schedule:File", false, IdfObjectFormat.NotSpecified, false, 5, false)},
            {"Material", new IdfObject("Material", false, IdfObjectFormat.NotSpecified, false, 6, false)},
            {"Material:NoMass", new IdfObject("Material:NoMass", false, IdfObjectFormat.NotSpecified, false, 3, false)},
            {
                "Material:InfraredTransparent",
                new IdfObject("Material:InfraredTransparent", false, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {"Material:AirGap", new IdfObject("Material:AirGap", false, IdfObjectFormat.NotSpecified, false, 2, false)},
            {
                "Material:RoofVegetation",
                new IdfObject("Material:RoofVegetation", false, IdfObjectFormat.NotSpecified, false, 19, false)
            },
            {
                "WindowMaterial:SimpleGlazingSystem",
                new IdfObject("WindowMaterial:SimpleGlazingSystem", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "WindowMaterial:Glazing",
                new IdfObject("WindowMaterial:Glazing", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "WindowMaterial:GlazingGroup:Thermochromic",
                new IdfObject("WindowMaterial:GlazingGroup:Thermochromic", false, IdfObjectFormat.NotSpecified, false,
                    3, false)
            },
            {
                "WindowMaterial:Glazing:RefractionExtinctionMethod",
                new IdfObject("WindowMaterial:Glazing:RefractionExtinctionMethod", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "WindowMaterial:Gas",
                new IdfObject("WindowMaterial:Gas", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "WindowGap:SupportPillar",
                new IdfObject("WindowGap:SupportPillar", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowGap:DeflectionState",
                new IdfObject("WindowGap:DeflectionState", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowMaterial:GasMixture",
                new IdfObject("WindowMaterial:GasMixture", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "WindowMaterial:Gap",
                new IdfObject("WindowMaterial:Gap", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowMaterial:Shade",
                new IdfObject("WindowMaterial:Shade", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "WindowMaterial:ComplexShade",
                new IdfObject("WindowMaterial:ComplexShade", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "WindowMaterial:Blind",
                new IdfObject("WindowMaterial:Blind", false, IdfObjectFormat.NotSpecified, false, 29, false)
            },
            {
                "WindowMaterial:Screen",
                new IdfObject("WindowMaterial:Screen", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "WindowMaterial:Shade:EquivalentLayer",
                new IdfObject("WindowMaterial:Shade:EquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 6,
                    false)
            },
            {
                "WindowMaterial:Drape:EquivalentLayer",
                new IdfObject("WindowMaterial:Drape:EquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "WindowMaterial:Blind:EquivalentLayer",
                new IdfObject("WindowMaterial:Blind:EquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 10,
                    false)
            },
            {
                "WindowMaterial:Screen:EquivalentLayer",
                new IdfObject("WindowMaterial:Screen:EquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 11,
                    false)
            },
            {
                "WindowMaterial:Glazing:EquivalentLayer",
                new IdfObject("WindowMaterial:Glazing:EquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 11,
                    false)
            },
            {
                "WindowMaterial:Gap:EquivalentLayer",
                new IdfObject("WindowMaterial:Gap:EquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "MaterialProperty:MoisturePenetrationDepth:Settings",
                new IdfObject("MaterialProperty:MoisturePenetrationDepth:Settings", false, IdfObjectFormat.NotSpecified,
                    false, 10, false)
            },
            {
                "MaterialProperty:PhaseChange",
                new IdfObject("MaterialProperty:PhaseChange", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "MaterialProperty:PhaseChangeHysteresis",
                new IdfObject("MaterialProperty:PhaseChangeHysteresis", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "MaterialProperty:VariableThermalConductivity",
                new IdfObject("MaterialProperty:VariableThermalConductivity", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "MaterialProperty:HeatAndMoistureTransfer:Settings",
                new IdfObject("MaterialProperty:HeatAndMoistureTransfer:Settings", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm",
                new IdfObject("MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "MaterialProperty:HeatAndMoistureTransfer:Suction",
                new IdfObject("MaterialProperty:HeatAndMoistureTransfer:Suction", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "MaterialProperty:HeatAndMoistureTransfer:Redistribution",
                new IdfObject("MaterialProperty:HeatAndMoistureTransfer:Redistribution", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "MaterialProperty:HeatAndMoistureTransfer:Diffusion",
                new IdfObject("MaterialProperty:HeatAndMoistureTransfer:Diffusion", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity",
                new IdfObject("MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "MaterialProperty:GlazingSpectralData",
                new IdfObject("MaterialProperty:GlazingSpectralData", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {"Construction", new IdfObject("Construction", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Construction:CfactorUndergroundWall",
                new IdfObject("Construction:CfactorUndergroundWall", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Construction:FfactorGroundFloor",
                new IdfObject("Construction:FfactorGroundFloor", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Construction:InternalSource",
                new IdfObject("Construction:InternalSource", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Construction:AirBoundary",
                new IdfObject("Construction:AirBoundary", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowThermalModel:Params",
                new IdfObject("WindowThermalModel:Params", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowsCalculationEngine",
                new IdfObject("WindowsCalculationEngine", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Construction:ComplexFenestrationState",
                new IdfObject("Construction:ComplexFenestrationState", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Construction:WindowEquivalentLayer",
                new IdfObject("Construction:WindowEquivalentLayer", false, IdfObjectFormat.NotSpecified, false, 2,
                    false)
            },
            {
                "Construction:WindowDataFile",
                new IdfObject("Construction:WindowDataFile", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GlobalGeometryRules",
                new IdfObject("GlobalGeometryRules", true, IdfObjectFormat.NotSpecified, false, -1, true)
            },
            {
                "GeometryTransform",
                new IdfObject("GeometryTransform", true, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {"Zone", new IdfObject("Zone", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"ZoneList", new IdfObject("ZoneList", false, IdfObjectFormat.NotSpecified, false, 2, false)},
            {"ZoneGroup", new IdfObject("ZoneGroup", false, IdfObjectFormat.NotSpecified, false, 2, false)},
            {
                "BuildingSurface:Detailed",
                new IdfObject("BuildingSurface:Detailed", false, IdfObjectFormat.NotSpecified, false, 19, false)
            },
            {"Wall:Detailed", new IdfObject("Wall:Detailed", false, IdfObjectFormat.NotSpecified, false, 18, false)},
            {
                "RoofCeiling:Detailed",
                new IdfObject("RoofCeiling:Detailed", false, IdfObjectFormat.NotSpecified, false, 18, false)
            },
            {"Floor:Detailed", new IdfObject("Floor:Detailed", false, IdfObjectFormat.NotSpecified, false, 18, false)},
            {"Wall:Exterior", new IdfObject("Wall:Exterior", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Wall:Adiabatic", new IdfObject("Wall:Adiabatic", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Wall:Underground",
                new IdfObject("Wall:Underground", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Wall:Interzone", new IdfObject("Wall:Interzone", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Roof", new IdfObject("Roof", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Ceiling:Adiabatic",
                new IdfObject("Ceiling:Adiabatic", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Ceiling:Interzone",
                new IdfObject("Ceiling:Interzone", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Floor:GroundContact",
                new IdfObject("Floor:GroundContact", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Floor:Adiabatic",
                new IdfObject("Floor:Adiabatic", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Floor:Interzone",
                new IdfObject("Floor:Interzone", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "FenestrationSurface:Detailed",
                new IdfObject("FenestrationSurface:Detailed", false, IdfObjectFormat.NotSpecified, false, 18, false)
            },
            {"Window", new IdfObject("Window", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Door", new IdfObject("Door", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"GlazedDoor", new IdfObject("GlazedDoor", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Window:Interzone",
                new IdfObject("Window:Interzone", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Door:Interzone", new IdfObject("Door:Interzone", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "GlazedDoor:Interzone",
                new IdfObject("GlazedDoor:Interzone", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowShadingControl",
                new IdfObject("WindowShadingControl", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WindowProperty:FrameAndDivider",
                new IdfObject("WindowProperty:FrameAndDivider", false, IdfObjectFormat.NotSpecified, false, 20, false)
            },
            {
                "WindowProperty:AirflowControl",
                new IdfObject("WindowProperty:AirflowControl", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "WindowProperty:StormWindow",
                new IdfObject("WindowProperty:StormWindow", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {"InternalMass", new IdfObject("InternalMass", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Shading:Site", new IdfObject("Shading:Site", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Shading:Building",
                new IdfObject("Shading:Building", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Shading:Site:Detailed",
                new IdfObject("Shading:Site:Detailed", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "Shading:Building:Detailed",
                new IdfObject("Shading:Building:Detailed", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "Shading:Overhang",
                new IdfObject("Shading:Overhang", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Shading:Overhang:Projection",
                new IdfObject("Shading:Overhang:Projection", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Shading:Fin", new IdfObject("Shading:Fin", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Shading:Fin:Projection",
                new IdfObject("Shading:Fin:Projection", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Shading:Zone:Detailed",
                new IdfObject("Shading:Zone:Detailed", false, IdfObjectFormat.NotSpecified, false, 13, false)
            },
            {
                "ShadingProperty:Reflectance",
                new IdfObject("ShadingProperty:Reflectance", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "SurfaceProperty:HeatTransferAlgorithm",
                new IdfObject("SurfaceProperty:HeatTransferAlgorithm", false, IdfObjectFormat.NotSpecified, false, 2,
                    false)
            },
            {
                "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface",
                new IdfObject("SurfaceProperty:HeatTransferAlgorithm:MultipleSurface", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "SurfaceProperty:HeatTransferAlgorithm:SurfaceList",
                new IdfObject("SurfaceProperty:HeatTransferAlgorithm:SurfaceList", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "SurfaceProperty:HeatTransferAlgorithm:Construction",
                new IdfObject("SurfaceProperty:HeatTransferAlgorithm:Construction", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "SurfaceProperty:HeatBalanceSourceTerm",
                new IdfObject("SurfaceProperty:HeatBalanceSourceTerm", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "SurfaceControl:MovableInsulation",
                new IdfObject("SurfaceControl:MovableInsulation", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SurfaceProperty:OtherSideCoefficients",
                new IdfObject("SurfaceProperty:OtherSideCoefficients", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "SurfaceProperty:OtherSideConditionsModel",
                new IdfObject("SurfaceProperty:OtherSideConditionsModel", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SurfaceProperty:Underwater",
                new IdfObject("SurfaceProperty:Underwater", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "Foundation:Kiva",
                new IdfObject("Foundation:Kiva", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Foundation:Kiva:Settings",
                new IdfObject("Foundation:Kiva:Settings", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SurfaceProperty:ExposedFoundationPerimeter",
                new IdfObject("SurfaceProperty:ExposedFoundationPerimeter", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections",
                new IdfObject("SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections", true,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections",
                new IdfObject("SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections", true,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SurfaceConvectionAlgorithm:Inside:UserCurve",
                new IdfObject("SurfaceConvectionAlgorithm:Inside:UserCurve", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SurfaceConvectionAlgorithm:Outside:UserCurve",
                new IdfObject("SurfaceConvectionAlgorithm:Outside:UserCurve", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "SurfaceProperty:ConvectionCoefficients",
                new IdfObject("SurfaceProperty:ConvectionCoefficients", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "SurfaceProperty:ConvectionCoefficients:MultipleSurface",
                new IdfObject("SurfaceProperty:ConvectionCoefficients:MultipleSurface", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SurfaceProperties:VaporCoefficients",
                new IdfObject("SurfaceProperties:VaporCoefficients", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "SurfaceProperty:ExteriorNaturalVentedCavity",
                new IdfObject("SurfaceProperty:ExteriorNaturalVentedCavity", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SurfaceProperty:SolarIncidentInside",
                new IdfObject("SurfaceProperty:SolarIncidentInside", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "SurfaceProperty:LocalEnvironment",
                new IdfObject("SurfaceProperty:LocalEnvironment", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ZoneProperty:LocalEnvironment",
                new IdfObject("ZoneProperty:LocalEnvironment", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "SurfaceProperty:SurroundingSurfaces",
                new IdfObject("SurfaceProperty:SurroundingSurfaces", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "ComplexFenestrationProperty:SolarAbsorbedLayers",
                new IdfObject("ComplexFenestrationProperty:SolarAbsorbedLayers", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "ZoneProperty:UserViewFactors:bySurfaceName",
                new IdfObject("ZoneProperty:UserViewFactors:bySurfaceName", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "GroundHeatTransfer:Control",
                new IdfObject("GroundHeatTransfer:Control", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatTransfer:Slab:Materials",
                new IdfObject("GroundHeatTransfer:Slab:Materials", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:MatlProps",
                new IdfObject("GroundHeatTransfer:Slab:MatlProps", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:BoundConds",
                new IdfObject("GroundHeatTransfer:Slab:BoundConds", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:BldgProps",
                new IdfObject("GroundHeatTransfer:Slab:BldgProps", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:Insulation",
                new IdfObject("GroundHeatTransfer:Slab:Insulation", false, IdfObjectFormat.NotSpecified, false, 5,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:EquivalentSlab",
                new IdfObject("GroundHeatTransfer:Slab:EquivalentSlab", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:AutoGrid",
                new IdfObject("GroundHeatTransfer:Slab:AutoGrid", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatTransfer:Slab:ManualGrid",
                new IdfObject("GroundHeatTransfer:Slab:ManualGrid", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Slab:XFACE",
                new IdfObject("GroundHeatTransfer:Slab:XFACE", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatTransfer:Slab:YFACE",
                new IdfObject("GroundHeatTransfer:Slab:YFACE", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatTransfer:Slab:ZFACE",
                new IdfObject("GroundHeatTransfer:Slab:ZFACE", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatTransfer:Basement:SimParameters",
                new IdfObject("GroundHeatTransfer:Basement:SimParameters", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "GroundHeatTransfer:Basement:MatlProps",
                new IdfObject("GroundHeatTransfer:Basement:MatlProps", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:Insulation",
                new IdfObject("GroundHeatTransfer:Basement:Insulation", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:SurfaceProps",
                new IdfObject("GroundHeatTransfer:Basement:SurfaceProps", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:BldgData",
                new IdfObject("GroundHeatTransfer:Basement:BldgData", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:Interior",
                new IdfObject("GroundHeatTransfer:Basement:Interior", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:ComBldg",
                new IdfObject("GroundHeatTransfer:Basement:ComBldg", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:EquivSlab",
                new IdfObject("GroundHeatTransfer:Basement:EquivSlab", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:EquivAutoGrid",
                new IdfObject("GroundHeatTransfer:Basement:EquivAutoGrid", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "GroundHeatTransfer:Basement:AutoGrid",
                new IdfObject("GroundHeatTransfer:Basement:AutoGrid", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:ManualGrid",
                new IdfObject("GroundHeatTransfer:Basement:ManualGrid", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:XFACE",
                new IdfObject("GroundHeatTransfer:Basement:XFACE", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:YFACE",
                new IdfObject("GroundHeatTransfer:Basement:YFACE", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatTransfer:Basement:ZFACE",
                new IdfObject("GroundHeatTransfer:Basement:ZFACE", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "RoomAirModelType",
                new IdfObject("RoomAirModelType", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "RoomAir:TemperaturePattern:UserDefined",
                new IdfObject("RoomAir:TemperaturePattern:UserDefined", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "RoomAir:TemperaturePattern:ConstantGradient",
                new IdfObject("RoomAir:TemperaturePattern:ConstantGradient", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "RoomAir:TemperaturePattern:TwoGradient",
                new IdfObject("RoomAir:TemperaturePattern:TwoGradient", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "RoomAir:TemperaturePattern:NondimensionalHeight",
                new IdfObject("RoomAir:TemperaturePattern:NondimensionalHeight", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "RoomAir:TemperaturePattern:SurfaceMapping",
                new IdfObject("RoomAir:TemperaturePattern:SurfaceMapping", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {"RoomAir:Node", new IdfObject("RoomAir:Node", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "RoomAirSettings:OneNodeDisplacementVentilation",
                new IdfObject("RoomAirSettings:OneNodeDisplacementVentilation", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "RoomAirSettings:ThreeNodeDisplacementVentilation",
                new IdfObject("RoomAirSettings:ThreeNodeDisplacementVentilation", false, IdfObjectFormat.NotSpecified,
                    false, 6, false)
            },
            {
                "RoomAirSettings:CrossVentilation",
                new IdfObject("RoomAirSettings:CrossVentilation", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "RoomAirSettings:UnderFloorAirDistributionInterior",
                new IdfObject("RoomAirSettings:UnderFloorAirDistributionInterior", false, IdfObjectFormat.NotSpecified,
                    false, 15, false)
            },
            {
                "RoomAirSettings:UnderFloorAirDistributionExterior",
                new IdfObject("RoomAirSettings:UnderFloorAirDistributionExterior", false, IdfObjectFormat.NotSpecified,
                    false, 15, false)
            },
            {
                "RoomAir:Node:AirflowNetwork",
                new IdfObject("RoomAir:Node:AirflowNetwork", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "RoomAir:Node:AirflowNetwork:AdjacentSurfaceList",
                new IdfObject("RoomAir:Node:AirflowNetwork:AdjacentSurfaceList", false, IdfObjectFormat.NotSpecified,
                    false, 2, false)
            },
            {
                "RoomAir:Node:AirflowNetwork:InternalGains",
                new IdfObject("RoomAir:Node:AirflowNetwork:InternalGains", false, IdfObjectFormat.NotSpecified, false,
                    4, false)
            },
            {
                "RoomAir:Node:AirflowNetwork:HVACEquipment",
                new IdfObject("RoomAir:Node:AirflowNetwork:HVACEquipment", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "RoomAirSettings:AirflowNetwork",
                new IdfObject("RoomAirSettings:AirflowNetwork", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {"People", new IdfObject("People", false, IdfObjectFormat.NotSpecified, false, 10, false)},
            {
                "ComfortViewFactorAngles",
                new IdfObject("ComfortViewFactorAngles", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Lights", new IdfObject("Lights", false, IdfObjectFormat.NotSpecified, false, 11, false)},
            {
                "ElectricEquipment",
                new IdfObject("ElectricEquipment", false, IdfObjectFormat.NotSpecified, false, 10, false)
            },
            {"GasEquipment", new IdfObject("GasEquipment", false, IdfObjectFormat.NotSpecified, false, 10, false)},
            {
                "HotWaterEquipment",
                new IdfObject("HotWaterEquipment", false, IdfObjectFormat.NotSpecified, false, 10, false)
            },
            {"SteamEquipment", new IdfObject("SteamEquipment", false, IdfObjectFormat.NotSpecified, false, 10, false)},
            {"OtherEquipment", new IdfObject("OtherEquipment", false, IdfObjectFormat.NotSpecified, false, 11, false)},
            {
                "ElectricEquipment:ITE:AirCooled",
                new IdfObject("ElectricEquipment:ITE:AirCooled", false, IdfObjectFormat.NotSpecified, false, 28, false)
            },
            {
                "ZoneBaseboard:OutdoorTemperatureControlled",
                new IdfObject("ZoneBaseboard:OutdoorTemperatureControlled", false, IdfObjectFormat.NotSpecified, false,
                    8, false)
            },
            {
                "SwimmingPool:Indoor",
                new IdfObject("SwimmingPool:Indoor", false, IdfObjectFormat.NotSpecified, false, 16, false)
            },
            {
                "ZoneContaminantSourceAndSink:CarbonDioxide",
                new IdfObject("ZoneContaminantSourceAndSink:CarbonDioxide", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "ZoneContaminantSourceAndSink:Generic:Constant",
                new IdfObject("ZoneContaminantSourceAndSink:Generic:Constant", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "SurfaceContaminantSourceAndSink:Generic:PressureDriven",
                new IdfObject("SurfaceContaminantSourceAndSink:Generic:PressureDriven", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneContaminantSourceAndSink:Generic:CutoffModel",
                new IdfObject("ZoneContaminantSourceAndSink:Generic:CutoffModel", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "ZoneContaminantSourceAndSink:Generic:DecaySource",
                new IdfObject("ZoneContaminantSourceAndSink:Generic:DecaySource", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion",
                new IdfObject("SurfaceContaminantSourceAndSink:Generic:BoundaryLayerDiffusion", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink",
                new IdfObject("SurfaceContaminantSourceAndSink:Generic:DepositionVelocitySink", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneContaminantSourceAndSink:Generic:DepositionRateSink",
                new IdfObject("ZoneContaminantSourceAndSink:Generic:DepositionRateSink", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Daylighting:Controls",
                new IdfObject("Daylighting:Controls", false, IdfObjectFormat.NotSpecified, false, 16, false)
            },
            {
                "Daylighting:ReferencePoint",
                new IdfObject("Daylighting:ReferencePoint", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Daylighting:DELight:ComplexFenestration",
                new IdfObject("Daylighting:DELight:ComplexFenestration", false, IdfObjectFormat.NotSpecified, false, 5,
                    false)
            },
            {
                "DaylightingDevice:Tubular",
                new IdfObject("DaylightingDevice:Tubular", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "DaylightingDevice:Shelf",
                new IdfObject("DaylightingDevice:Shelf", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DaylightingDevice:LightWell",
                new IdfObject("DaylightingDevice:LightWell", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Output:DaylightFactors",
                new IdfObject("Output:DaylightFactors", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:IlluminanceMap",
                new IdfObject("Output:IlluminanceMap", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "OutputControl:IlluminanceMap:Style",
                new IdfObject("OutputControl:IlluminanceMap:Style", true, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ZoneInfiltration:DesignFlowRate",
                new IdfObject("ZoneInfiltration:DesignFlowRate", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "ZoneInfiltration:EffectiveLeakageArea",
                new IdfObject("ZoneInfiltration:EffectiveLeakageArea", false, IdfObjectFormat.NotSpecified, false, 6,
                    false)
            },
            {
                "ZoneInfiltration:FlowCoefficient",
                new IdfObject("ZoneInfiltration:FlowCoefficient", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "ZoneVentilation:DesignFlowRate",
                new IdfObject("ZoneVentilation:DesignFlowRate", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "ZoneVentilation:WindandStackOpenArea",
                new IdfObject("ZoneVentilation:WindandStackOpenArea", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "ZoneAirBalance:OutdoorAir",
                new IdfObject("ZoneAirBalance:OutdoorAir", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"ZoneMixing", new IdfObject("ZoneMixing", false, IdfObjectFormat.NotSpecified, false, 9, false)},
            {"ZoneCrossMixing", new IdfObject("ZoneCrossMixing", false, IdfObjectFormat.NotSpecified, false, 9, false)},
            {
                "ZoneRefrigerationDoorMixing",
                new IdfObject("ZoneRefrigerationDoorMixing", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {"ZoneEarthtube", new IdfObject("ZoneEarthtube", false, IdfObjectFormat.NotSpecified, false, 22, false)},
            {
                "ZoneCoolTower:Shower",
                new IdfObject("ZoneCoolTower:Shower", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneThermalChimney",
                new IdfObject("ZoneThermalChimney", false, IdfObjectFormat.NotSpecified, false, 10, false)
            },
            {
                "AirflowNetwork:SimulationControl",
                new IdfObject("AirflowNetwork:SimulationControl", true, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "AirflowNetwork:MultiZone:Zone",
                new IdfObject("AirflowNetwork:MultiZone:Zone", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "AirflowNetwork:MultiZone:Surface",
                new IdfObject("AirflowNetwork:MultiZone:Surface", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "AirflowNetwork:MultiZone:ReferenceCrackConditions",
                new IdfObject("AirflowNetwork:MultiZone:ReferenceCrackConditions", false, IdfObjectFormat.NotSpecified,
                    false, 4, false)
            },
            {
                "AirflowNetwork:MultiZone:Surface:Crack",
                new IdfObject("AirflowNetwork:MultiZone:Surface:Crack", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea",
                new IdfObject("AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea", false,
                    IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "AirflowNetwork:MultiZone:Component:DetailedOpening",
                new IdfObject("AirflowNetwork:MultiZone:Component:DetailedOpening", false, IdfObjectFormat.NotSpecified,
                    false, 16, false)
            },
            {
                "AirflowNetwork:MultiZone:Component:SimpleOpening",
                new IdfObject("AirflowNetwork:MultiZone:Component:SimpleOpening", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "AirflowNetwork:MultiZone:Component:HorizontalOpening",
                new IdfObject("AirflowNetwork:MultiZone:Component:HorizontalOpening", false,
                    IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "AirflowNetwork:MultiZone:Component:ZoneExhaustFan",
                new IdfObject("AirflowNetwork:MultiZone:Component:ZoneExhaustFan", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "AirflowNetwork:MultiZone:ExternalNode",
                new IdfObject("AirflowNetwork:MultiZone:ExternalNode", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "AirflowNetwork:MultiZone:WindPressureCoefficientArray",
                new IdfObject("AirflowNetwork:MultiZone:WindPressureCoefficientArray", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "AirflowNetwork:MultiZone:WindPressureCoefficientValues",
                new IdfObject("AirflowNetwork:MultiZone:WindPressureCoefficientValues", false,
                    IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "AirflowNetwork:ZoneControl:PressureController",
                new IdfObject("AirflowNetwork:ZoneControl:PressureController", true, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "AirflowNetwork:Distribution:Node",
                new IdfObject("AirflowNetwork:Distribution:Node", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "AirflowNetwork:Distribution:Component:Leak",
                new IdfObject("AirflowNetwork:Distribution:Component:Leak", false, IdfObjectFormat.NotSpecified, false,
                    3, false)
            },
            {
                "AirflowNetwork:Distribution:Component:LeakageRatio",
                new IdfObject("AirflowNetwork:Distribution:Component:LeakageRatio", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "AirflowNetwork:Distribution:Component:Duct",
                new IdfObject("AirflowNetwork:Distribution:Component:Duct", false, IdfObjectFormat.NotSpecified, false,
                    8, false)
            },
            {
                "AirflowNetwork:Distribution:Component:Fan",
                new IdfObject("AirflowNetwork:Distribution:Component:Fan", false, IdfObjectFormat.NotSpecified, false,
                    2, false)
            },
            {
                "AirflowNetwork:Distribution:Component:Coil",
                new IdfObject("AirflowNetwork:Distribution:Component:Coil", false, IdfObjectFormat.NotSpecified, false,
                    4, false)
            },
            {
                "AirflowNetwork:Distribution:Component:HeatExchanger",
                new IdfObject("AirflowNetwork:Distribution:Component:HeatExchanger", false,
                    IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "AirflowNetwork:Distribution:Component:TerminalUnit",
                new IdfObject("AirflowNetwork:Distribution:Component:TerminalUnit", false, IdfObjectFormat.NotSpecified,
                    false, 4, false)
            },
            {
                "AirflowNetwork:Distribution:Component:ConstantPressureDrop",
                new IdfObject("AirflowNetwork:Distribution:Component:ConstantPressureDrop", false,
                    IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "AirflowNetwork:Distribution:Component:OutdoorAirFlow",
                new IdfObject("AirflowNetwork:Distribution:Component:OutdoorAirFlow", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "AirflowNetwork:Distribution:Component:ReliefAirFlow",
                new IdfObject("AirflowNetwork:Distribution:Component:ReliefAirFlow", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "AirflowNetwork:Distribution:Linkage",
                new IdfObject("AirflowNetwork:Distribution:Linkage", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "AirflowNetwork:Distribution:DuctViewFactors",
                new IdfObject("AirflowNetwork:Distribution:DuctViewFactors", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AirflowNetwork:OccupantVentilationControl",
                new IdfObject("AirflowNetwork:OccupantVentilationControl", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AirflowNetwork:IntraZone:Node",
                new IdfObject("AirflowNetwork:IntraZone:Node", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "AirflowNetwork:IntraZone:Linkage",
                new IdfObject("AirflowNetwork:IntraZone:Linkage", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "Exterior:Lights",
                new IdfObject("Exterior:Lights", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Exterior:FuelEquipment",
                new IdfObject("Exterior:FuelEquipment", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Exterior:WaterEquipment",
                new IdfObject("Exterior:WaterEquipment", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "HVACTemplate:Thermostat",
                new IdfObject("HVACTemplate:Thermostat", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "HVACTemplate:Zone:IdealLoadsAirSystem",
                new IdfObject("HVACTemplate:Zone:IdealLoadsAirSystem", false, IdfObjectFormat.NotSpecified, false, 26,
                    false)
            },
            {
                "HVACTemplate:Zone:BaseboardHeat",
                new IdfObject("HVACTemplate:Zone:BaseboardHeat", false, IdfObjectFormat.NotSpecified, false, 11, false)
            },
            {
                "HVACTemplate:Zone:FanCoil",
                new IdfObject("HVACTemplate:Zone:FanCoil", false, IdfObjectFormat.NotSpecified, false, 34, false)
            },
            {
                "HVACTemplate:Zone:PTAC",
                new IdfObject("HVACTemplate:Zone:PTAC", false, IdfObjectFormat.NotSpecified, false, 40, false)
            },
            {
                "HVACTemplate:Zone:PTHP",
                new IdfObject("HVACTemplate:Zone:PTHP", false, IdfObjectFormat.NotSpecified, false, 50, false)
            },
            {
                "HVACTemplate:Zone:WaterToAirHeatPump",
                new IdfObject("HVACTemplate:Zone:WaterToAirHeatPump", false, IdfObjectFormat.NotSpecified, false, 44,
                    false)
            },
            {
                "HVACTemplate:Zone:VRF",
                new IdfObject("HVACTemplate:Zone:VRF", false, IdfObjectFormat.NotSpecified, false, 44, false)
            },
            {
                "HVACTemplate:Zone:Unitary",
                new IdfObject("HVACTemplate:Zone:Unitary", false, IdfObjectFormat.NotSpecified, false, 21, false)
            },
            {
                "HVACTemplate:Zone:VAV",
                new IdfObject("HVACTemplate:Zone:VAV", false, IdfObjectFormat.NotSpecified, false, 32, false)
            },
            {
                "HVACTemplate:Zone:VAV:FanPowered",
                new IdfObject("HVACTemplate:Zone:VAV:FanPowered", false, IdfObjectFormat.NotSpecified, false, 31, false)
            },
            {
                "HVACTemplate:Zone:VAV:HeatAndCool",
                new IdfObject("HVACTemplate:Zone:VAV:HeatAndCool", false, IdfObjectFormat.NotSpecified, false, 27,
                    false)
            },
            {
                "HVACTemplate:Zone:ConstantVolume",
                new IdfObject("HVACTemplate:Zone:ConstantVolume", false, IdfObjectFormat.NotSpecified, false, 24, false)
            },
            {
                "HVACTemplate:Zone:DualDuct",
                new IdfObject("HVACTemplate:Zone:DualDuct", false, IdfObjectFormat.NotSpecified, false, 26, false)
            },
            {
                "HVACTemplate:System:VRF",
                new IdfObject("HVACTemplate:System:VRF", false, IdfObjectFormat.NotSpecified, false, 39, false)
            },
            {
                "HVACTemplate:System:Unitary",
                new IdfObject("HVACTemplate:System:Unitary", false, IdfObjectFormat.NotSpecified, false, 51, false)
            },
            {
                "HVACTemplate:System:UnitaryHeatPump:AirToAir",
                new IdfObject("HVACTemplate:System:UnitaryHeatPump:AirToAir", false, IdfObjectFormat.NotSpecified,
                    false, 61, false)
            },
            {
                "HVACTemplate:System:UnitarySystem",
                new IdfObject("HVACTemplate:System:UnitarySystem", false, IdfObjectFormat.NotSpecified, false, 71,
                    false)
            },
            {
                "HVACTemplate:System:VAV",
                new IdfObject("HVACTemplate:System:VAV", false, IdfObjectFormat.NotSpecified, false, 61, false)
            },
            {
                "HVACTemplate:System:PackagedVAV",
                new IdfObject("HVACTemplate:System:PackagedVAV", false, IdfObjectFormat.NotSpecified, false, 59, false)
            },
            {
                "HVACTemplate:System:ConstantVolume",
                new IdfObject("HVACTemplate:System:ConstantVolume", false, IdfObjectFormat.NotSpecified, false, 70,
                    false)
            },
            {
                "HVACTemplate:System:DualDuct",
                new IdfObject("HVACTemplate:System:DualDuct", false, IdfObjectFormat.NotSpecified, false, 91, false)
            },
            {
                "HVACTemplate:System:DedicatedOutdoorAir",
                new IdfObject("HVACTemplate:System:DedicatedOutdoorAir", false, IdfObjectFormat.NotSpecified, false, 46,
                    false)
            },
            {
                "HVACTemplate:Plant:ChilledWaterLoop",
                new IdfObject("HVACTemplate:Plant:ChilledWaterLoop", true, IdfObjectFormat.NotSpecified, false, 21,
                    false)
            },
            {
                "HVACTemplate:Plant:Chiller",
                new IdfObject("HVACTemplate:Plant:Chiller", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "HVACTemplate:Plant:Chiller:ObjectReference",
                new IdfObject("HVACTemplate:Plant:Chiller:ObjectReference", false, IdfObjectFormat.NotSpecified, false,
                    4, false)
            },
            {
                "HVACTemplate:Plant:Tower",
                new IdfObject("HVACTemplate:Plant:Tower", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "HVACTemplate:Plant:Tower:ObjectReference",
                new IdfObject("HVACTemplate:Plant:Tower:ObjectReference", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "HVACTemplate:Plant:HotWaterLoop",
                new IdfObject("HVACTemplate:Plant:HotWaterLoop", true, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "HVACTemplate:Plant:Boiler",
                new IdfObject("HVACTemplate:Plant:Boiler", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "HVACTemplate:Plant:Boiler:ObjectReference",
                new IdfObject("HVACTemplate:Plant:Boiler:ObjectReference", false, IdfObjectFormat.NotSpecified, false,
                    4, false)
            },
            {
                "HVACTemplate:Plant:MixedWaterLoop",
                new IdfObject("HVACTemplate:Plant:MixedWaterLoop", true, IdfObjectFormat.NotSpecified, false, 11, false)
            },
            {
                "DesignSpecification:OutdoorAir",
                new IdfObject("DesignSpecification:OutdoorAir", false, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {
                "DesignSpecification:ZoneAirDistribution",
                new IdfObject("DesignSpecification:ZoneAirDistribution", false, IdfObjectFormat.NotSpecified, false, 1,
                    false)
            },
            {
                "Sizing:Parameters",
                new IdfObject("Sizing:Parameters", true, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {"Sizing:Zone", new IdfObject("Sizing:Zone", false, IdfObjectFormat.NotSpecified, false, 18, false)},
            {
                "DesignSpecification:ZoneHVAC:Sizing",
                new IdfObject("DesignSpecification:ZoneHVAC:Sizing", false, IdfObjectFormat.NotSpecified, false, 1,
                    false)
            },
            {
                "DesignSpecification:AirTerminal:Sizing",
                new IdfObject("DesignSpecification:AirTerminal:Sizing", false, IdfObjectFormat.NotSpecified, false, 1,
                    false)
            },
            {"Sizing:System", new IdfObject("Sizing:System", false, IdfObjectFormat.NotSpecified, false, 37, false)},
            {"Sizing:Plant", new IdfObject("Sizing:Plant", false, IdfObjectFormat.NotSpecified, false, 4, false)},
            {
                "OutputControl:Sizing:Style",
                new IdfObject("OutputControl:Sizing:Style", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneControl:Humidistat",
                new IdfObject("ZoneControl:Humidistat", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ZoneControl:Thermostat",
                new IdfObject("ZoneControl:Thermostat", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneControl:Thermostat:OperativeTemperature",
                new IdfObject("ZoneControl:Thermostat:OperativeTemperature", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "ZoneControl:Thermostat:ThermalComfort",
                new IdfObject("ZoneControl:Thermostat:ThermalComfort", false, IdfObjectFormat.NotSpecified, false, 9,
                    false)
            },
            {
                "ZoneControl:Thermostat:TemperatureAndHumidity",
                new IdfObject("ZoneControl:Thermostat:TemperatureAndHumidity", false, IdfObjectFormat.NotSpecified,
                    false, 2, false)
            },
            {
                "ThermostatSetpoint:SingleHeating",
                new IdfObject("ThermostatSetpoint:SingleHeating", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ThermostatSetpoint:SingleCooling",
                new IdfObject("ThermostatSetpoint:SingleCooling", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ThermostatSetpoint:SingleHeatingOrCooling",
                new IdfObject("ThermostatSetpoint:SingleHeatingOrCooling", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "ThermostatSetpoint:DualSetpoint",
                new IdfObject("ThermostatSetpoint:DualSetpoint", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating",
                new IdfObject("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating", false,
                    IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling",
                new IdfObject("ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling", false,
                    IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling",
                new IdfObject("ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling", false,
                    IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint",
                new IdfObject("ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ZoneControl:Thermostat:StagedDualSetpoint",
                new IdfObject("ZoneControl:Thermostat:StagedDualSetpoint", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "ZoneControl:ContaminantController",
                new IdfObject("ZoneControl:ContaminantController", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "ZoneHVAC:IdealLoadsAirSystem",
                new IdfObject("ZoneHVAC:IdealLoadsAirSystem", false, IdfObjectFormat.NotSpecified, false, 27, false)
            },
            {
                "ZoneHVAC:FourPipeFanCoil",
                new IdfObject("ZoneHVAC:FourPipeFanCoil", false, IdfObjectFormat.NotSpecified, false, 24, false)
            },
            {
                "ZoneHVAC:WindowAirConditioner",
                new IdfObject("ZoneHVAC:WindowAirConditioner", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "ZoneHVAC:PackagedTerminalAirConditioner",
                new IdfObject("ZoneHVAC:PackagedTerminalAirConditioner", false, IdfObjectFormat.NotSpecified, false, 18,
                    false)
            },
            {
                "ZoneHVAC:PackagedTerminalHeatPump",
                new IdfObject("ZoneHVAC:PackagedTerminalHeatPump", false, IdfObjectFormat.NotSpecified, false, 26,
                    false)
            },
            {
                "ZoneHVAC:WaterToAirHeatPump",
                new IdfObject("ZoneHVAC:WaterToAirHeatPump", false, IdfObjectFormat.NotSpecified, false, 25, false)
            },
            {
                "ZoneHVAC:Dehumidifier:DX",
                new IdfObject("ZoneHVAC:Dehumidifier:DX", false, IdfObjectFormat.NotSpecified, false, 13, false)
            },
            {
                "ZoneHVAC:EnergyRecoveryVentilator",
                new IdfObject("ZoneHVAC:EnergyRecoveryVentilator", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "ZoneHVAC:EnergyRecoveryVentilator:Controller",
                new IdfObject("ZoneHVAC:EnergyRecoveryVentilator:Controller", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "ZoneHVAC:UnitVentilator",
                new IdfObject("ZoneHVAC:UnitVentilator", false, IdfObjectFormat.NotSpecified, false, 16, false)
            },
            {
                "ZoneHVAC:UnitHeater",
                new IdfObject("ZoneHVAC:UnitHeater", false, IdfObjectFormat.NotSpecified, false, 11, false)
            },
            {
                "ZoneHVAC:EvaporativeCoolerUnit",
                new IdfObject("ZoneHVAC:EvaporativeCoolerUnit", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "ZoneHVAC:HybridUnitaryHVAC",
                new IdfObject("ZoneHVAC:HybridUnitaryHVAC", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ZoneHVAC:OutdoorAirUnit",
                new IdfObject("ZoneHVAC:OutdoorAirUnit", false, IdfObjectFormat.NotSpecified, false, 18, false)
            },
            {
                "ZoneHVAC:OutdoorAirUnit:EquipmentList",
                new IdfObject("ZoneHVAC:OutdoorAirUnit:EquipmentList", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow",
                new IdfObject("ZoneHVAC:TerminalUnit:VariableRefrigerantFlow", false, IdfObjectFormat.NotSpecified,
                    false, 19, false)
            },
            {
                "ZoneHVAC:Baseboard:RadiantConvective:Water",
                new IdfObject("ZoneHVAC:Baseboard:RadiantConvective:Water", false, IdfObjectFormat.NotSpecified, false,
                    12, false)
            },
            {
                "ZoneHVAC:Baseboard:RadiantConvective:Steam",
                new IdfObject("ZoneHVAC:Baseboard:RadiantConvective:Steam", false, IdfObjectFormat.NotSpecified, false,
                    11, false)
            },
            {
                "ZoneHVAC:Baseboard:RadiantConvective:Electric",
                new IdfObject("ZoneHVAC:Baseboard:RadiantConvective:Electric", false, IdfObjectFormat.NotSpecified,
                    false, 8, false)
            },
            {
                "ZoneHVAC:CoolingPanel:RadiantConvective:Water",
                new IdfObject("ZoneHVAC:CoolingPanel:RadiantConvective:Water", false, IdfObjectFormat.NotSpecified,
                    false, 18, false)
            },
            {
                "ZoneHVAC:Baseboard:Convective:Water",
                new IdfObject("ZoneHVAC:Baseboard:Convective:Water", false, IdfObjectFormat.NotSpecified, false, 10,
                    false)
            },
            {
                "ZoneHVAC:Baseboard:Convective:Electric",
                new IdfObject("ZoneHVAC:Baseboard:Convective:Electric", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
                new IdfObject("ZoneHVAC:LowTemperatureRadiant:VariableFlow", false, IdfObjectFormat.NotSpecified, false,
                    29, false)
            },
            {
                "ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
                new IdfObject("ZoneHVAC:LowTemperatureRadiant:ConstantFlow", false, IdfObjectFormat.NotSpecified, false,
                    29, false)
            },
            {
                "ZoneHVAC:LowTemperatureRadiant:Electric",
                new IdfObject("ZoneHVAC:LowTemperatureRadiant:Electric", false, IdfObjectFormat.NotSpecified, false, 11,
                    false)
            },
            {
                "ZoneHVAC:LowTemperatureRadiant:SurfaceGroup",
                new IdfObject("ZoneHVAC:LowTemperatureRadiant:SurfaceGroup", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "ZoneHVAC:HighTemperatureRadiant",
                new IdfObject("ZoneHVAC:HighTemperatureRadiant", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "ZoneHVAC:VentilatedSlab",
                new IdfObject("ZoneHVAC:VentilatedSlab", false, IdfObjectFormat.NotSpecified, false, 32, false)
            },
            {
                "ZoneHVAC:VentilatedSlab:SlabGroup",
                new IdfObject("ZoneHVAC:VentilatedSlab:SlabGroup", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "AirTerminal:SingleDuct:Uncontrolled",
                new IdfObject("AirTerminal:SingleDuct:Uncontrolled", false, IdfObjectFormat.NotSpecified, true, 4,
                    false)
            },
            {
                "AirTerminal:SingleDuct:ConstantVolume:Reheat",
                new IdfObject("AirTerminal:SingleDuct:ConstantVolume:Reheat", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
                new IdfObject("AirTerminal:SingleDuct:ConstantVolume:NoReheat", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "AirTerminal:SingleDuct:VAV:NoReheat",
                new IdfObject("AirTerminal:SingleDuct:VAV:NoReheat", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "AirTerminal:SingleDuct:VAV:Reheat",
                new IdfObject("AirTerminal:SingleDuct:VAV:Reheat", false, IdfObjectFormat.NotSpecified, false, 18,
                    false)
            },
            {
                "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan",
                new IdfObject("AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat",
                new IdfObject("AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat", false, IdfObjectFormat.NotSpecified,
                    false, 6, false)
            },
            {
                "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat",
                new IdfObject("AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat", false, IdfObjectFormat.NotSpecified,
                    false, 11, false)
            },
            {
                "AirTerminal:SingleDuct:SeriesPIU:Reheat",
                new IdfObject("AirTerminal:SingleDuct:SeriesPIU:Reheat", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "AirTerminal:SingleDuct:ParallelPIU:Reheat",
                new IdfObject("AirTerminal:SingleDuct:ParallelPIU:Reheat", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction",
                new IdfObject("AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction", false,
                    IdfObjectFormat.NotSpecified, false, 18, false)
            },
            {
                "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam",
                new IdfObject("AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "AirTerminal:SingleDuct:ConstantVolume:CooledBeam",
                new IdfObject("AirTerminal:SingleDuct:ConstantVolume:CooledBeam", false, IdfObjectFormat.NotSpecified,
                    false, 23, false)
            },
            {
                "AirTerminal:SingleDuct:Mixer",
                new IdfObject("AirTerminal:SingleDuct:Mixer", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirTerminal:DualDuct:ConstantVolume",
                new IdfObject("AirTerminal:DualDuct:ConstantVolume", false, IdfObjectFormat.NotSpecified, false, 6,
                    false)
            },
            {
                "AirTerminal:DualDuct:VAV",
                new IdfObject("AirTerminal:DualDuct:VAV", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "AirTerminal:DualDuct:VAV:OutdoorAir",
                new IdfObject("AirTerminal:DualDuct:VAV:OutdoorAir", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "ZoneHVAC:AirDistributionUnit",
                new IdfObject("ZoneHVAC:AirDistributionUnit", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "ZoneHVAC:EquipmentList",
                new IdfObject("ZoneHVAC:EquipmentList", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "ZoneHVAC:EquipmentConnections",
                new IdfObject("ZoneHVAC:EquipmentConnections", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Fan:SystemModel",
                new IdfObject("Fan:SystemModel", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "Fan:ConstantVolume",
                new IdfObject("Fan:ConstantVolume", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "Fan:VariableVolume",
                new IdfObject("Fan:VariableVolume", false, IdfObjectFormat.NotSpecified, false, 17, false)
            },
            {"Fan:OnOff", new IdfObject("Fan:OnOff", false, IdfObjectFormat.NotSpecified, false, 9, false)},
            {"Fan:ZoneExhaust", new IdfObject("Fan:ZoneExhaust", false, IdfObjectFormat.NotSpecified, false, 7, false)},
            {
                "FanPerformance:NightVentilation",
                new IdfObject("FanPerformance:NightVentilation", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Fan:ComponentModel",
                new IdfObject("Fan:ComponentModel", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Coil:Cooling:Water",
                new IdfObject("Coil:Cooling:Water", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "Coil:Cooling:Water:DetailedGeometry",
                new IdfObject("Coil:Cooling:Water:DetailedGeometry", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Coil:Cooling:DX:SingleSpeed",
                new IdfObject("Coil:Cooling:DX:SingleSpeed", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "Coil:Cooling:DX:TwoSpeed",
                new IdfObject("Coil:Cooling:DX:TwoSpeed", false, IdfObjectFormat.NotSpecified, false, 20, false)
            },
            {
                "Coil:Cooling:DX:MultiSpeed",
                new IdfObject("Coil:Cooling:DX:MultiSpeed", false, IdfObjectFormat.NotSpecified, false, 56, false)
            },
            {
                "Coil:Cooling:DX:VariableSpeed",
                new IdfObject("Coil:Cooling:DX:VariableSpeed", false, IdfObjectFormat.NotSpecified, false, 31, false)
            },
            {
                "Coil:Cooling:DX:TwoStageWithHumidityControlMode",
                new IdfObject("Coil:Cooling:DX:TwoStageWithHumidityControlMode", false, IdfObjectFormat.NotSpecified,
                    false, 10, false)
            },
            {
                "CoilPerformance:DX:Cooling",
                new IdfObject("CoilPerformance:DX:Cooling", false, IdfObjectFormat.NotSpecified, false, 11, false)
            },
            {
                "Coil:Cooling:DX:VariableRefrigerantFlow",
                new IdfObject("Coil:Cooling:DX:VariableRefrigerantFlow", false, IdfObjectFormat.NotSpecified, false, 9,
                    false)
            },
            {
                "Coil:Heating:DX:VariableRefrigerantFlow",
                new IdfObject("Coil:Heating:DX:VariableRefrigerantFlow", false, IdfObjectFormat.NotSpecified, false, 5,
                    false)
            },
            {
                "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl",
                new IdfObject("Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl", false,
                    IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl",
                new IdfObject("Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl", false,
                    IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Coil:Heating:Water",
                new IdfObject("Coil:Heating:Water", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "Coil:Heating:Steam",
                new IdfObject("Coil:Heating:Steam", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Coil:Heating:Electric",
                new IdfObject("Coil:Heating:Electric", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Coil:Heating:Electric:MultiStage",
                new IdfObject("Coil:Heating:Electric:MultiStage", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Coil:Heating:Fuel",
                new IdfObject("Coil:Heating:Fuel", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "Coil:Heating:Gas:MultiStage",
                new IdfObject("Coil:Heating:Gas:MultiStage", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "Coil:Heating:Desuperheater",
                new IdfObject("Coil:Heating:Desuperheater", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "Coil:Heating:DX:SingleSpeed",
                new IdfObject("Coil:Heating:DX:SingleSpeed", false, IdfObjectFormat.NotSpecified, false, 21, false)
            },
            {
                "Coil:Heating:DX:MultiSpeed",
                new IdfObject("Coil:Heating:DX:MultiSpeed", false, IdfObjectFormat.NotSpecified, false, 40, false)
            },
            {
                "Coil:Heating:DX:VariableSpeed",
                new IdfObject("Coil:Heating:DX:VariableSpeed", false, IdfObjectFormat.NotSpecified, false, 25, false)
            },
            {
                "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation",
                new IdfObject("Coil:Cooling:WaterToAirHeatPump:ParameterEstimation", false,
                    IdfObjectFormat.NotSpecified, false, 18, false)
            },
            {
                "Coil:Heating:WaterToAirHeatPump:ParameterEstimation",
                new IdfObject("Coil:Heating:WaterToAirHeatPump:ParameterEstimation", false,
                    IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "Coil:Cooling:WaterToAirHeatPump:EquationFit",
                new IdfObject("Coil:Cooling:WaterToAirHeatPump:EquationFit", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit",
                new IdfObject("Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit", false,
                    IdfObjectFormat.NotSpecified, false, 27, false)
            },
            {
                "Coil:Heating:WaterToAirHeatPump:EquationFit",
                new IdfObject("Coil:Heating:WaterToAirHeatPump:EquationFit", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit",
                new IdfObject("Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit", false,
                    IdfObjectFormat.NotSpecified, false, 23, false)
            },
            {
                "Coil:WaterHeating:AirToWaterHeatPump:Pumped",
                new IdfObject("Coil:WaterHeating:AirToWaterHeatPump:Pumped", false, IdfObjectFormat.NotSpecified, false,
                    21, false)
            },
            {
                "Coil:WaterHeating:AirToWaterHeatPump:Wrapped",
                new IdfObject("Coil:WaterHeating:AirToWaterHeatPump:Wrapped", false, IdfObjectFormat.NotSpecified,
                    false, 14, false)
            },
            {
                "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed",
                new IdfObject("Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed", false, IdfObjectFormat.NotSpecified,
                    false, 33, false)
            },
            {
                "Coil:WaterHeating:Desuperheater",
                new IdfObject("Coil:WaterHeating:Desuperheater", false, IdfObjectFormat.NotSpecified, false, 18, false)
            },
            {
                "CoilSystem:Cooling:DX",
                new IdfObject("CoilSystem:Cooling:DX", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "CoilSystem:Heating:DX",
                new IdfObject("CoilSystem:Heating:DX", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "CoilSystem:Cooling:Water:HeatExchangerAssisted",
                new IdfObject("CoilSystem:Cooling:Water:HeatExchangerAssisted", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "CoilSystem:Cooling:DX:HeatExchangerAssisted",
                new IdfObject("CoilSystem:Cooling:DX:HeatExchangerAssisted", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "CoilSystem:IntegratedHeatPump:AirSource",
                new IdfObject("CoilSystem:IntegratedHeatPump:AirSource", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                new IdfObject("Coil:Cooling:DX:SingleSpeed:ThermalStorage", false, IdfObjectFormat.NotSpecified, false,
                    89, false)
            },
            {
                "EvaporativeCooler:Direct:CelDekPad",
                new IdfObject("EvaporativeCooler:Direct:CelDekPad", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "EvaporativeCooler:Indirect:CelDekPad",
                new IdfObject("EvaporativeCooler:Indirect:CelDekPad", false, IdfObjectFormat.NotSpecified, false, 14,
                    false)
            },
            {
                "EvaporativeCooler:Indirect:WetCoil",
                new IdfObject("EvaporativeCooler:Indirect:WetCoil", false, IdfObjectFormat.NotSpecified, false, 13,
                    false)
            },
            {
                "EvaporativeCooler:Indirect:ResearchSpecial",
                new IdfObject("EvaporativeCooler:Indirect:ResearchSpecial", false, IdfObjectFormat.NotSpecified, false,
                    21, false)
            },
            {
                "EvaporativeCooler:Direct:ResearchSpecial",
                new IdfObject("EvaporativeCooler:Direct:ResearchSpecial", false, IdfObjectFormat.NotSpecified, false,
                    11, false)
            },
            {
                "Humidifier:Steam:Electric",
                new IdfObject("Humidifier:Steam:Electric", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Humidifier:Steam:Gas",
                new IdfObject("Humidifier:Steam:Gas", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Dehumidifier:Desiccant:NoFans",
                new IdfObject("Dehumidifier:Desiccant:NoFans", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Dehumidifier:Desiccant:System",
                new IdfObject("Dehumidifier:Desiccant:System", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "HeatExchanger:AirToAir:FlatPlate",
                new IdfObject("HeatExchanger:AirToAir:FlatPlate", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "HeatExchanger:AirToAir:SensibleAndLatent",
                new IdfObject("HeatExchanger:AirToAir:SensibleAndLatent", false, IdfObjectFormat.NotSpecified, false,
                    19, false)
            },
            {
                "HeatExchanger:Desiccant:BalancedFlow",
                new IdfObject("HeatExchanger:Desiccant:BalancedFlow", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1",
                new IdfObject("HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1", false,
                    IdfObjectFormat.NotSpecified, false, 52, false)
            },
            {
                "AirLoopHVAC:UnitarySystem",
                new IdfObject("AirLoopHVAC:UnitarySystem", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "UnitarySystemPerformance:Multispeed",
                new IdfObject("UnitarySystemPerformance:Multispeed", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "AirLoopHVAC:Unitary:Furnace:HeatOnly",
                new IdfObject("AirLoopHVAC:Unitary:Furnace:HeatOnly", false, IdfObjectFormat.NotSpecified, false, 13,
                    false)
            },
            {
                "AirLoopHVAC:Unitary:Furnace:HeatCool",
                new IdfObject("AirLoopHVAC:Unitary:Furnace:HeatCool", false, IdfObjectFormat.NotSpecified, false, 17,
                    false)
            },
            {
                "AirLoopHVAC:UnitaryHeatOnly",
                new IdfObject("AirLoopHVAC:UnitaryHeatOnly", false, IdfObjectFormat.NotSpecified, false, 13, false)
            },
            {
                "AirLoopHVAC:UnitaryHeatCool",
                new IdfObject("AirLoopHVAC:UnitaryHeatCool", false, IdfObjectFormat.NotSpecified, false, 17, false)
            },
            {
                "AirLoopHVAC:UnitaryHeatPump:AirToAir",
                new IdfObject("AirLoopHVAC:UnitaryHeatPump:AirToAir", false, IdfObjectFormat.NotSpecified, false, 19,
                    false)
            },
            {
                "AirLoopHVAC:UnitaryHeatPump:WaterToAir",
                new IdfObject("AirLoopHVAC:UnitaryHeatPump:WaterToAir", false, IdfObjectFormat.NotSpecified, false, 25,
                    false)
            },
            {
                "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass",
                new IdfObject("AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass", false, IdfObjectFormat.NotSpecified,
                    false, 23, false)
            },
            {
                "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed",
                new IdfObject("AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed", false, IdfObjectFormat.NotSpecified,
                    false, 31, false)
            },
            {
                "AirConditioner:VariableRefrigerantFlow",
                new IdfObject("AirConditioner:VariableRefrigerantFlow", false, IdfObjectFormat.NotSpecified, false, 37,
                    false)
            },
            {
                "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl",
                new IdfObject("AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl", false,
                    IdfObjectFormat.NotSpecified, false, 41, false)
            },
            {
                "AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR",
                new IdfObject("AirConditioner:VariableRefrigerantFlow:FluidTemperatureControl:HR", false,
                    IdfObjectFormat.NotSpecified, false, 58, false)
            },
            {
                "ZoneTerminalUnitList",
                new IdfObject("ZoneTerminalUnitList", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "Controller:WaterCoil",
                new IdfObject("Controller:WaterCoil", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "Controller:OutdoorAir",
                new IdfObject("Controller:OutdoorAir", false, IdfObjectFormat.NotSpecified, false, 16, false)
            },
            {
                "Controller:MechanicalVentilation",
                new IdfObject("Controller:MechanicalVentilation", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "AirLoopHVAC:ControllerList",
                new IdfObject("AirLoopHVAC:ControllerList", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"AirLoopHVAC", new IdfObject("AirLoopHVAC", false, IdfObjectFormat.NotSpecified, false, 10, false)},
            {
                "AirLoopHVAC:OutdoorAirSystem:EquipmentList",
                new IdfObject("AirLoopHVAC:OutdoorAirSystem:EquipmentList", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AirLoopHVAC:OutdoorAirSystem",
                new IdfObject("AirLoopHVAC:OutdoorAirSystem", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "OutdoorAir:Mixer",
                new IdfObject("OutdoorAir:Mixer", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:ZoneSplitter",
                new IdfObject("AirLoopHVAC:ZoneSplitter", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:SupplyPlenum",
                new IdfObject("AirLoopHVAC:SupplyPlenum", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:SupplyPath",
                new IdfObject("AirLoopHVAC:SupplyPath", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:ZoneMixer",
                new IdfObject("AirLoopHVAC:ZoneMixer", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:ReturnPlenum",
                new IdfObject("AirLoopHVAC:ReturnPlenum", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:ReturnPath",
                new IdfObject("AirLoopHVAC:ReturnPath", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:DedicatedOutdoorAirSystem",
                new IdfObject("AirLoopHVAC:DedicatedOutdoorAirSystem", false, IdfObjectFormat.NotSpecified, false, 11,
                    false)
            },
            {
                "AirLoopHVAC:Mixer",
                new IdfObject("AirLoopHVAC:Mixer", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AirLoopHVAC:Splitter",
                new IdfObject("AirLoopHVAC:Splitter", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Branch", new IdfObject("Branch", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"BranchList", new IdfObject("BranchList", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Connector:Splitter",
                new IdfObject("Connector:Splitter", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {"Connector:Mixer", new IdfObject("Connector:Mixer", false, IdfObjectFormat.NotSpecified, false, 3, false)},
            {"ConnectorList", new IdfObject("ConnectorList", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"NodeList", new IdfObject("NodeList", false, IdfObjectFormat.NotSpecified, false, 2, false)},
            {
                "OutdoorAir:Node",
                new IdfObject("OutdoorAir:Node", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "OutdoorAir:NodeList",
                new IdfObject("OutdoorAir:NodeList", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Pipe:Adiabatic", new IdfObject("Pipe:Adiabatic", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Pipe:Adiabatic:Steam",
                new IdfObject("Pipe:Adiabatic:Steam", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Pipe:Indoor", new IdfObject("Pipe:Indoor", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Pipe:Outdoor", new IdfObject("Pipe:Outdoor", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Pipe:Underground",
                new IdfObject("Pipe:Underground", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "PipingSystem:Underground:Domain",
                new IdfObject("PipingSystem:Underground:Domain", false, IdfObjectFormat.NotSpecified, false, 31, false)
            },
            {
                "PipingSystem:Underground:PipeCircuit",
                new IdfObject("PipingSystem:Underground:PipeCircuit", false, IdfObjectFormat.NotSpecified, false, 15,
                    false)
            },
            {
                "PipingSystem:Underground:PipeSegment",
                new IdfObject("PipingSystem:Underground:PipeSegment", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {"Duct", new IdfObject("Duct", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Pump:VariableSpeed",
                new IdfObject("Pump:VariableSpeed", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "Pump:ConstantSpeed",
                new IdfObject("Pump:ConstantSpeed", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "Pump:VariableSpeed:Condensate",
                new IdfObject("Pump:VariableSpeed:Condensate", false, IdfObjectFormat.NotSpecified, false, 13, false)
            },
            {
                "HeaderedPumps:ConstantSpeed",
                new IdfObject("HeaderedPumps:ConstantSpeed", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "HeaderedPumps:VariableSpeed",
                new IdfObject("HeaderedPumps:VariableSpeed", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {"TemperingValve", new IdfObject("TemperingValve", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "LoadProfile:Plant",
                new IdfObject("LoadProfile:Plant", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SolarCollectorPerformance:FlatPlate",
                new IdfObject("SolarCollectorPerformance:FlatPlate", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "SolarCollector:FlatPlate:Water",
                new IdfObject("SolarCollector:FlatPlate:Water", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SolarCollector:FlatPlate:PhotovoltaicThermal",
                new IdfObject("SolarCollector:FlatPlate:PhotovoltaicThermal", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "SolarCollectorPerformance:PhotovoltaicThermal:Simple",
                new IdfObject("SolarCollectorPerformance:PhotovoltaicThermal:Simple", false,
                    IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SolarCollector:IntegralCollectorStorage",
                new IdfObject("SolarCollector:IntegralCollectorStorage", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "SolarCollectorPerformance:IntegralCollectorStorage",
                new IdfObject("SolarCollectorPerformance:IntegralCollectorStorage", false, IdfObjectFormat.NotSpecified,
                    false, 19, false)
            },
            {
                "SolarCollector:UnglazedTranspired",
                new IdfObject("SolarCollector:UnglazedTranspired", false, IdfObjectFormat.NotSpecified, false, 23,
                    false)
            },
            {
                "SolarCollector:UnglazedTranspired:Multisystem",
                new IdfObject("SolarCollector:UnglazedTranspired:Multisystem", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "Boiler:HotWater",
                new IdfObject("Boiler:HotWater", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {"Boiler:Steam", new IdfObject("Boiler:Steam", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Chiller:Electric:EIR",
                new IdfObject("Chiller:Electric:EIR", false, IdfObjectFormat.NotSpecified, false, 23, false)
            },
            {
                "Chiller:Electric:ReformulatedEIR",
                new IdfObject("Chiller:Electric:ReformulatedEIR", false, IdfObjectFormat.NotSpecified, false, 22, false)
            },
            {
                "Chiller:Electric",
                new IdfObject("Chiller:Electric", false, IdfObjectFormat.NotSpecified, false, 27, false)
            },
            {
                "Chiller:Absorption:Indirect",
                new IdfObject("Chiller:Absorption:Indirect", false, IdfObjectFormat.NotSpecified, false, 17, false)
            },
            {
                "Chiller:Absorption",
                new IdfObject("Chiller:Absorption", false, IdfObjectFormat.NotSpecified, false, 23, false)
            },
            {
                "Chiller:ConstantCOP",
                new IdfObject("Chiller:ConstantCOP", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "Chiller:EngineDriven",
                new IdfObject("Chiller:EngineDriven", false, IdfObjectFormat.NotSpecified, false, 43, false)
            },
            {
                "Chiller:CombustionTurbine",
                new IdfObject("Chiller:CombustionTurbine", false, IdfObjectFormat.NotSpecified, false, 56, false)
            },
            {
                "ChillerHeater:Absorption:DirectFired",
                new IdfObject("ChillerHeater:Absorption:DirectFired", false, IdfObjectFormat.NotSpecified, false, 34,
                    false)
            },
            {
                "ChillerHeater:Absorption:DoubleEffect",
                new IdfObject("ChillerHeater:Absorption:DoubleEffect", false, IdfObjectFormat.NotSpecified, false, 34,
                    false)
            },
            {
                "HeatPump:WaterToWater:EIR:Cooling",
                new IdfObject("HeatPump:WaterToWater:EIR:Cooling", false, IdfObjectFormat.NotSpecified, false, 15,
                    false)
            },
            {
                "HeatPump:WaterToWater:EIR:Heating",
                new IdfObject("HeatPump:WaterToWater:EIR:Heating", false, IdfObjectFormat.NotSpecified, false, 15,
                    false)
            },
            {
                "HeatPump:WaterToWater:EquationFit:Heating",
                new IdfObject("HeatPump:WaterToWater:EquationFit:Heating", false, IdfObjectFormat.NotSpecified, false,
                    19, false)
            },
            {
                "HeatPump:WaterToWater:EquationFit:Cooling",
                new IdfObject("HeatPump:WaterToWater:EquationFit:Cooling", false, IdfObjectFormat.NotSpecified, false,
                    19, false)
            },
            {
                "HeatPump:WaterToWater:ParameterEstimation:Cooling",
                new IdfObject("HeatPump:WaterToWater:ParameterEstimation:Cooling", false, IdfObjectFormat.NotSpecified,
                    false, 20, false)
            },
            {
                "HeatPump:WaterToWater:ParameterEstimation:Heating",
                new IdfObject("HeatPump:WaterToWater:ParameterEstimation:Heating", false, IdfObjectFormat.NotSpecified,
                    false, 20, false)
            },
            {
                "DistrictCooling",
                new IdfObject("DistrictCooling", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DistrictHeating",
                new IdfObject("DistrictHeating", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "PlantComponent:TemperatureSource",
                new IdfObject("PlantComponent:TemperatureSource", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "CentralHeatPumpSystem",
                new IdfObject("CentralHeatPumpSystem", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "ChillerHeaterPerformance:Electric:EIR",
                new IdfObject("ChillerHeaterPerformance:Electric:EIR", false, IdfObjectFormat.NotSpecified, false, 29,
                    false)
            },
            {
                "CoolingTower:SingleSpeed",
                new IdfObject("CoolingTower:SingleSpeed", false, IdfObjectFormat.NotSpecified, false, 16, false)
            },
            {
                "CoolingTower:TwoSpeed",
                new IdfObject("CoolingTower:TwoSpeed", false, IdfObjectFormat.NotSpecified, false, 24, false)
            },
            {
                "CoolingTower:VariableSpeed:Merkel",
                new IdfObject("CoolingTower:VariableSpeed:Merkel", false, IdfObjectFormat.NotSpecified, false, 24,
                    false)
            },
            {
                "CoolingTower:VariableSpeed",
                new IdfObject("CoolingTower:VariableSpeed", false, IdfObjectFormat.NotSpecified, false, 15, false)
            },
            {
                "CoolingTowerPerformance:CoolTools",
                new IdfObject("CoolingTowerPerformance:CoolTools", false, IdfObjectFormat.NotSpecified, false, 44,
                    false)
            },
            {
                "CoolingTowerPerformance:YorkCalc",
                new IdfObject("CoolingTowerPerformance:YorkCalc", false, IdfObjectFormat.NotSpecified, false, 37, false)
            },
            {
                "EvaporativeFluidCooler:SingleSpeed",
                new IdfObject("EvaporativeFluidCooler:SingleSpeed", false, IdfObjectFormat.NotSpecified, false, 10,
                    false)
            },
            {
                "EvaporativeFluidCooler:TwoSpeed",
                new IdfObject("EvaporativeFluidCooler:TwoSpeed", false, IdfObjectFormat.NotSpecified, false, 23, false)
            },
            {
                "FluidCooler:SingleSpeed",
                new IdfObject("FluidCooler:SingleSpeed", false, IdfObjectFormat.NotSpecified, false, 12, false)
            },
            {
                "FluidCooler:TwoSpeed",
                new IdfObject("FluidCooler:TwoSpeed", false, IdfObjectFormat.NotSpecified, false, 20, false)
            },
            {
                "GroundHeatExchanger:System",
                new IdfObject("GroundHeatExchanger:System", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "GroundHeatExchanger:Vertical:Properties",
                new IdfObject("GroundHeatExchanger:Vertical:Properties", false, IdfObjectFormat.NotSpecified, false, 11,
                    false)
            },
            {
                "GroundHeatExchanger:Vertical:Array",
                new IdfObject("GroundHeatExchanger:Vertical:Array", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatExchanger:Vertical:Single",
                new IdfObject("GroundHeatExchanger:Vertical:Single", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "GroundHeatExchanger:ResponseFactors",
                new IdfObject("GroundHeatExchanger:ResponseFactors", false, IdfObjectFormat.NotSpecified, false, 5,
                    false)
            },
            {
                "GroundHeatExchanger:Pond",
                new IdfObject("GroundHeatExchanger:Pond", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatExchanger:Surface",
                new IdfObject("GroundHeatExchanger:Surface", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "GroundHeatExchanger:HorizontalTrench",
                new IdfObject("GroundHeatExchanger:HorizontalTrench", false, IdfObjectFormat.NotSpecified, false, 20,
                    false)
            },
            {
                "GroundHeatExchanger:Slinky",
                new IdfObject("GroundHeatExchanger:Slinky", false, IdfObjectFormat.NotSpecified, false, 21, false)
            },
            {
                "HeatExchanger:FluidToFluid",
                new IdfObject("HeatExchanger:FluidToFluid", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "WaterHeater:Mixed",
                new IdfObject("WaterHeater:Mixed", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WaterHeater:Stratified",
                new IdfObject("WaterHeater:Stratified", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WaterHeater:Sizing",
                new IdfObject("WaterHeater:Sizing", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "WaterHeater:HeatPump:PumpedCondenser",
                new IdfObject("WaterHeater:HeatPump:PumpedCondenser", false, IdfObjectFormat.NotSpecified, false, 32,
                    false)
            },
            {
                "WaterHeater:HeatPump:WrappedCondenser",
                new IdfObject("WaterHeater:HeatPump:WrappedCondenser", false, IdfObjectFormat.NotSpecified, false, 31,
                    false)
            },
            {
                "ThermalStorage:Ice:Simple",
                new IdfObject("ThermalStorage:Ice:Simple", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "ThermalStorage:Ice:Detailed",
                new IdfObject("ThermalStorage:Ice:Detailed", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "ThermalStorage:ChilledWater:Mixed",
                new IdfObject("ThermalStorage:ChilledWater:Mixed", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ThermalStorage:ChilledWater:Stratified",
                new IdfObject("ThermalStorage:ChilledWater:Stratified", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {"PlantLoop", new IdfObject("PlantLoop", false, IdfObjectFormat.NotSpecified, false, 18, false)},
            {"CondenserLoop", new IdfObject("CondenserLoop", false, IdfObjectFormat.NotSpecified, false, 18, false)},
            {
                "PlantEquipmentList",
                new IdfObject("PlantEquipmentList", false, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {
                "CondenserEquipmentList",
                new IdfObject("CondenserEquipmentList", false, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {
                "PlantEquipmentOperation:Uncontrolled",
                new IdfObject("PlantEquipmentOperation:Uncontrolled", false, IdfObjectFormat.NotSpecified, false, 2,
                    false)
            },
            {
                "PlantEquipmentOperation:CoolingLoad",
                new IdfObject("PlantEquipmentOperation:CoolingLoad", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "PlantEquipmentOperation:HeatingLoad",
                new IdfObject("PlantEquipmentOperation:HeatingLoad", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "PlantEquipmentOperation:OutdoorDryBulb",
                new IdfObject("PlantEquipmentOperation:OutdoorDryBulb", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "PlantEquipmentOperation:OutdoorWetBulb",
                new IdfObject("PlantEquipmentOperation:OutdoorWetBulb", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "PlantEquipmentOperation:OutdoorRelativeHumidity",
                new IdfObject("PlantEquipmentOperation:OutdoorRelativeHumidity", false, IdfObjectFormat.NotSpecified,
                    false, 4, false)
            },
            {
                "PlantEquipmentOperation:OutdoorDewpoint",
                new IdfObject("PlantEquipmentOperation:OutdoorDewpoint", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "PlantEquipmentOperation:ComponentSetpoint",
                new IdfObject("PlantEquipmentOperation:ComponentSetpoint", false, IdfObjectFormat.NotSpecified, false,
                    7, false)
            },
            {
                "PlantEquipmentOperation:ThermalEnergyStorage",
                new IdfObject("PlantEquipmentOperation:ThermalEnergyStorage", false, IdfObjectFormat.NotSpecified,
                    false, 7, false)
            },
            {
                "PlantEquipmentOperation:OutdoorDryBulbDifference",
                new IdfObject("PlantEquipmentOperation:OutdoorDryBulbDifference", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "PlantEquipmentOperation:OutdoorWetBulbDifference",
                new IdfObject("PlantEquipmentOperation:OutdoorWetBulbDifference", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "PlantEquipmentOperation:OutdoorDewpointDifference",
                new IdfObject("PlantEquipmentOperation:OutdoorDewpointDifference", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "PlantEquipmentOperationSchemes",
                new IdfObject("PlantEquipmentOperationSchemes", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "CondenserEquipmentOperationSchemes",
                new IdfObject("CondenserEquipmentOperationSchemes", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "EnergyManagementSystem:Sensor",
                new IdfObject("EnergyManagementSystem:Sensor", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "EnergyManagementSystem:Actuator",
                new IdfObject("EnergyManagementSystem:Actuator", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "EnergyManagementSystem:ProgramCallingManager",
                new IdfObject("EnergyManagementSystem:ProgramCallingManager", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "EnergyManagementSystem:Program",
                new IdfObject("EnergyManagementSystem:Program", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "EnergyManagementSystem:Subroutine",
                new IdfObject("EnergyManagementSystem:Subroutine", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "EnergyManagementSystem:GlobalVariable",
                new IdfObject("EnergyManagementSystem:GlobalVariable", false, IdfObjectFormat.NotSpecified, false, 1,
                    false)
            },
            {
                "EnergyManagementSystem:OutputVariable",
                new IdfObject("EnergyManagementSystem:OutputVariable", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "EnergyManagementSystem:MeteredOutputVariable",
                new IdfObject("EnergyManagementSystem:MeteredOutputVariable", false, IdfObjectFormat.NotSpecified,
                    false, 7, false)
            },
            {
                "EnergyManagementSystem:TrendVariable",
                new IdfObject("EnergyManagementSystem:TrendVariable", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "EnergyManagementSystem:InternalVariable",
                new IdfObject("EnergyManagementSystem:InternalVariable", false, IdfObjectFormat.NotSpecified, false, 3,
                    false)
            },
            {
                "EnergyManagementSystem:CurveOrTableIndexVariable",
                new IdfObject("EnergyManagementSystem:CurveOrTableIndexVariable", false, IdfObjectFormat.NotSpecified,
                    false, 2, false)
            },
            {
                "EnergyManagementSystem:ConstructionIndexVariable",
                new IdfObject("EnergyManagementSystem:ConstructionIndexVariable", false, IdfObjectFormat.NotSpecified,
                    false, 2, false)
            },
            {
                "ExternalInterface",
                new IdfObject("ExternalInterface", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ExternalInterface:Schedule",
                new IdfObject("ExternalInterface:Schedule", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ExternalInterface:Variable",
                new IdfObject("ExternalInterface:Variable", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ExternalInterface:Actuator",
                new IdfObject("ExternalInterface:Actuator", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitImport",
                new IdfObject("ExternalInterface:FunctionalMockupUnitImport", false, IdfObjectFormat.NotSpecified,
                    false, 3, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitImport:From:Variable",
                new IdfObject("ExternalInterface:FunctionalMockupUnitImport:From:Variable", false,
                    IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitImport:To:Schedule",
                new IdfObject("ExternalInterface:FunctionalMockupUnitImport:To:Schedule", false,
                    IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitImport:To:Actuator",
                new IdfObject("ExternalInterface:FunctionalMockupUnitImport:To:Actuator", false,
                    IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitImport:To:Variable",
                new IdfObject("ExternalInterface:FunctionalMockupUnitImport:To:Variable", false,
                    IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitExport:From:Variable",
                new IdfObject("ExternalInterface:FunctionalMockupUnitExport:From:Variable", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitExport:To:Schedule",
                new IdfObject("ExternalInterface:FunctionalMockupUnitExport:To:Schedule", false,
                    IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitExport:To:Actuator",
                new IdfObject("ExternalInterface:FunctionalMockupUnitExport:To:Actuator", false,
                    IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "ExternalInterface:FunctionalMockupUnitExport:To:Variable",
                new IdfObject("ExternalInterface:FunctionalMockupUnitExport:To:Variable", false,
                    IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "ZoneHVAC:ForcedAir:UserDefined",
                new IdfObject("ZoneHVAC:ForcedAir:UserDefined", false, IdfObjectFormat.NotSpecified, false, 8, false)
            },
            {
                "AirTerminal:SingleDuct:UserDefined",
                new IdfObject("AirTerminal:SingleDuct:UserDefined", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "Coil:UserDefined",
                new IdfObject("Coil:UserDefined", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "PlantComponent:UserDefined",
                new IdfObject("PlantComponent:UserDefined", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "PlantEquipmentOperation:UserDefined",
                new IdfObject("PlantEquipmentOperation:UserDefined", false, IdfObjectFormat.NotSpecified, false, 5,
                    false)
            },
            {
                "AvailabilityManager:Scheduled",
                new IdfObject("AvailabilityManager:Scheduled", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "AvailabilityManager:ScheduledOn",
                new IdfObject("AvailabilityManager:ScheduledOn", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "AvailabilityManager:ScheduledOff",
                new IdfObject("AvailabilityManager:ScheduledOff", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "AvailabilityManager:OptimumStart",
                new IdfObject("AvailabilityManager:OptimumStart", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "AvailabilityManager:NightCycle",
                new IdfObject("AvailabilityManager:NightCycle", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "AvailabilityManager:DifferentialThermostat",
                new IdfObject("AvailabilityManager:DifferentialThermostat", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AvailabilityManager:HighTemperatureTurnOff",
                new IdfObject("AvailabilityManager:HighTemperatureTurnOff", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AvailabilityManager:HighTemperatureTurnOn",
                new IdfObject("AvailabilityManager:HighTemperatureTurnOn", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AvailabilityManager:LowTemperatureTurnOff",
                new IdfObject("AvailabilityManager:LowTemperatureTurnOff", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AvailabilityManager:LowTemperatureTurnOn",
                new IdfObject("AvailabilityManager:LowTemperatureTurnOn", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "AvailabilityManager:NightVentilation",
                new IdfObject("AvailabilityManager:NightVentilation", false, IdfObjectFormat.NotSpecified, false, 7,
                    false)
            },
            {
                "AvailabilityManager:HybridVentilation",
                new IdfObject("AvailabilityManager:HybridVentilation", false, IdfObjectFormat.NotSpecified, false, 13,
                    false)
            },
            {
                "AvailabilityManagerAssignmentList",
                new IdfObject("AvailabilityManagerAssignmentList", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "SetpointManager:Scheduled",
                new IdfObject("SetpointManager:Scheduled", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SetpointManager:Scheduled:DualSetpoint",
                new IdfObject("SetpointManager:Scheduled:DualSetpoint", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "SetpointManager:OutdoorAirReset",
                new IdfObject("SetpointManager:OutdoorAirReset", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SetpointManager:SingleZone:Reheat",
                new IdfObject("SetpointManager:SingleZone:Reheat", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "SetpointManager:SingleZone:Heating",
                new IdfObject("SetpointManager:SingleZone:Heating", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "SetpointManager:SingleZone:Cooling",
                new IdfObject("SetpointManager:SingleZone:Cooling", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "SetpointManager:SingleZone:Humidity:Minimum",
                new IdfObject("SetpointManager:SingleZone:Humidity:Minimum", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SetpointManager:SingleZone:Humidity:Maximum",
                new IdfObject("SetpointManager:SingleZone:Humidity:Maximum", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SetpointManager:MixedAir",
                new IdfObject("SetpointManager:MixedAir", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SetpointManager:OutdoorAirPretreat",
                new IdfObject("SetpointManager:OutdoorAirPretreat", false, IdfObjectFormat.NotSpecified, false, 11,
                    false)
            },
            {
                "SetpointManager:Warmest",
                new IdfObject("SetpointManager:Warmest", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SetpointManager:Coldest",
                new IdfObject("SetpointManager:Coldest", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "SetpointManager:ReturnAirBypassFlow",
                new IdfObject("SetpointManager:ReturnAirBypassFlow", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "SetpointManager:WarmestTemperatureFlow",
                new IdfObject("SetpointManager:WarmestTemperatureFlow", false, IdfObjectFormat.NotSpecified, false, 8,
                    false)
            },
            {
                "SetpointManager:MultiZone:Heating:Average",
                new IdfObject("SetpointManager:MultiZone:Heating:Average", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "SetpointManager:MultiZone:Cooling:Average",
                new IdfObject("SetpointManager:MultiZone:Cooling:Average", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "SetpointManager:MultiZone:MinimumHumidity:Average",
                new IdfObject("SetpointManager:MultiZone:MinimumHumidity:Average", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "SetpointManager:MultiZone:MaximumHumidity:Average",
                new IdfObject("SetpointManager:MultiZone:MaximumHumidity:Average", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "SetpointManager:MultiZone:Humidity:Minimum",
                new IdfObject("SetpointManager:MultiZone:Humidity:Minimum", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "SetpointManager:MultiZone:Humidity:Maximum",
                new IdfObject("SetpointManager:MultiZone:Humidity:Maximum", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "SetpointManager:FollowOutdoorAirTemperature",
                new IdfObject("SetpointManager:FollowOutdoorAirTemperature", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SetpointManager:FollowSystemNodeTemperature",
                new IdfObject("SetpointManager:FollowSystemNodeTemperature", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "SetpointManager:FollowGroundTemperature",
                new IdfObject("SetpointManager:FollowGroundTemperature", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "SetpointManager:CondenserEnteringReset",
                new IdfObject("SetpointManager:CondenserEnteringReset", false, IdfObjectFormat.NotSpecified, false, 10,
                    false)
            },
            {
                "SetpointManager:CondenserEnteringReset:Ideal",
                new IdfObject("SetpointManager:CondenserEnteringReset:Ideal", false, IdfObjectFormat.NotSpecified,
                    false, 5, false)
            },
            {
                "SetpointManager:SingleZone:OneStageCooling",
                new IdfObject("SetpointManager:SingleZone:OneStageCooling", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "SetpointManager:SingleZone:OneStageHeating",
                new IdfObject("SetpointManager:SingleZone:OneStageHeating", false, IdfObjectFormat.NotSpecified, false,
                    5, false)
            },
            {
                "SetpointManager:ReturnTemperature:ChilledWater",
                new IdfObject("SetpointManager:ReturnTemperature:ChilledWater", false, IdfObjectFormat.NotSpecified,
                    false, 7, false)
            },
            {
                "SetpointManager:ReturnTemperature:HotWater",
                new IdfObject("SetpointManager:ReturnTemperature:HotWater", false, IdfObjectFormat.NotSpecified, false,
                    7, false)
            },
            {
                "Refrigeration:Case",
                new IdfObject("Refrigeration:Case", false, IdfObjectFormat.NotSpecified, false, 28, false)
            },
            {
                "Refrigeration:CompressorRack",
                new IdfObject("Refrigeration:CompressorRack", false, IdfObjectFormat.NotSpecified, false, 25, false)
            },
            {
                "Refrigeration:CaseAndWalkInList",
                new IdfObject("Refrigeration:CaseAndWalkInList", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Refrigeration:Condenser:AirCooled",
                new IdfObject("Refrigeration:Condenser:AirCooled", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Refrigeration:Condenser:EvaporativeCooled",
                new IdfObject("Refrigeration:Condenser:EvaporativeCooled", false, IdfObjectFormat.NotSpecified, false,
                    10, false)
            },
            {
                "Refrigeration:Condenser:WaterCooled",
                new IdfObject("Refrigeration:Condenser:WaterCooled", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Refrigeration:Condenser:Cascade",
                new IdfObject("Refrigeration:Condenser:Cascade", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Refrigeration:GasCooler:AirCooled",
                new IdfObject("Refrigeration:GasCooler:AirCooled", false, IdfObjectFormat.NotSpecified, false, 4, false)
            },
            {
                "Refrigeration:TransferLoadList",
                new IdfObject("Refrigeration:TransferLoadList", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Refrigeration:Subcooler",
                new IdfObject("Refrigeration:Subcooler", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Refrigeration:Compressor",
                new IdfObject("Refrigeration:Compressor", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "Refrigeration:CompressorList",
                new IdfObject("Refrigeration:CompressorList", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "Refrigeration:System",
                new IdfObject("Refrigeration:System", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "Refrigeration:TranscriticalSystem",
                new IdfObject("Refrigeration:TranscriticalSystem", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Refrigeration:SecondarySystem",
                new IdfObject("Refrigeration:SecondarySystem", false, IdfObjectFormat.NotSpecified, false, 14, false)
            },
            {
                "Refrigeration:WalkIn",
                new IdfObject("Refrigeration:WalkIn", false, IdfObjectFormat.NotSpecified, false, 28, false)
            },
            {
                "Refrigeration:AirChiller",
                new IdfObject("Refrigeration:AirChiller", false, IdfObjectFormat.NotSpecified, false, 23, false)
            },
            {
                "ZoneHVAC:RefrigerationChillerSet",
                new IdfObject("ZoneHVAC:RefrigerationChillerSet", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "DemandManagerAssignmentList",
                new IdfObject("DemandManagerAssignmentList", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DemandManager:ExteriorLights",
                new IdfObject("DemandManager:ExteriorLights", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DemandManager:Lights",
                new IdfObject("DemandManager:Lights", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DemandManager:ElectricEquipment",
                new IdfObject("DemandManager:ElectricEquipment", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DemandManager:Thermostats",
                new IdfObject("DemandManager:Thermostats", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "DemandManager:Ventilation",
                new IdfObject("DemandManager:Ventilation", false, IdfObjectFormat.NotSpecified, false, 10, false)
            },
            {
                "Generator:InternalCombustionEngine",
                new IdfObject("Generator:InternalCombustionEngine", false, IdfObjectFormat.NotSpecified, false, 20,
                    false)
            },
            {
                "Generator:CombustionTurbine",
                new IdfObject("Generator:CombustionTurbine", false, IdfObjectFormat.NotSpecified, false, 22, false)
            },
            {
                "Generator:MicroTurbine",
                new IdfObject("Generator:MicroTurbine", false, IdfObjectFormat.NotSpecified, false, 11, false)
            },
            {
                "Generator:Photovoltaic",
                new IdfObject("Generator:Photovoltaic", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "PhotovoltaicPerformance:Simple",
                new IdfObject("PhotovoltaicPerformance:Simple", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "PhotovoltaicPerformance:EquivalentOne-Diode",
                new IdfObject("PhotovoltaicPerformance:EquivalentOne-Diode", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "PhotovoltaicPerformance:Sandia",
                new IdfObject("PhotovoltaicPerformance:Sandia", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:PVWatts",
                new IdfObject("Generator:PVWatts", false, IdfObjectFormat.NotSpecified, false, 9, false)
            },
            {
                "ElectricLoadCenter:Inverter:PVWatts",
                new IdfObject("ElectricLoadCenter:Inverter:PVWatts", false, IdfObjectFormat.NotSpecified, false, 1,
                    false)
            },
            {
                "Generator:FuelCell",
                new IdfObject("Generator:FuelCell", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:FuelCell:PowerModule",
                new IdfObject("Generator:FuelCell:PowerModule", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:FuelCell:AirSupply",
                new IdfObject("Generator:FuelCell:AirSupply", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:FuelCell:WaterSupply",
                new IdfObject("Generator:FuelCell:WaterSupply", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:FuelCell:AuxiliaryHeater",
                new IdfObject("Generator:FuelCell:AuxiliaryHeater", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Generator:FuelCell:ExhaustGasToWaterHeatExchanger",
                new IdfObject("Generator:FuelCell:ExhaustGasToWaterHeatExchanger", false, IdfObjectFormat.NotSpecified,
                    false, -1, false)
            },
            {
                "Generator:FuelCell:ElectricalStorage",
                new IdfObject("Generator:FuelCell:ElectricalStorage", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "Generator:FuelCell:Inverter",
                new IdfObject("Generator:FuelCell:Inverter", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:FuelCell:StackCooler",
                new IdfObject("Generator:FuelCell:StackCooler", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:MicroCHP",
                new IdfObject("Generator:MicroCHP", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:MicroCHP:NonNormalizedParameters",
                new IdfObject("Generator:MicroCHP:NonNormalizedParameters", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "Generator:FuelSupply",
                new IdfObject("Generator:FuelSupply", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Generator:WindTurbine",
                new IdfObject("Generator:WindTurbine", false, IdfObjectFormat.NotSpecified, false, 26, false)
            },
            {
                "ElectricLoadCenter:Generators",
                new IdfObject("ElectricLoadCenter:Generators", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "ElectricLoadCenter:Inverter:Simple",
                new IdfObject("ElectricLoadCenter:Inverter:Simple", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ElectricLoadCenter:Inverter:FunctionOfPower",
                new IdfObject("ElectricLoadCenter:Inverter:FunctionOfPower", false, IdfObjectFormat.NotSpecified, false,
                    -1, false)
            },
            {
                "ElectricLoadCenter:Inverter:LookUpTable",
                new IdfObject("ElectricLoadCenter:Inverter:LookUpTable", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ElectricLoadCenter:Storage:Simple",
                new IdfObject("ElectricLoadCenter:Storage:Simple", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ElectricLoadCenter:Storage:Battery",
                new IdfObject("ElectricLoadCenter:Storage:Battery", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "ElectricLoadCenter:Transformer",
                new IdfObject("ElectricLoadCenter:Transformer", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ElectricLoadCenter:Distribution",
                new IdfObject("ElectricLoadCenter:Distribution", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ElectricLoadCenter:Storage:Converter",
                new IdfObject("ElectricLoadCenter:Storage:Converter", false, IdfObjectFormat.NotSpecified, false, 4,
                    false)
            },
            {
                "WaterUse:Equipment",
                new IdfObject("WaterUse:Equipment", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WaterUse:Connections",
                new IdfObject("WaterUse:Connections", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "WaterUse:Storage",
                new IdfObject("WaterUse:Storage", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"WaterUse:Well", new IdfObject("WaterUse:Well", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "WaterUse:RainCollector",
                new IdfObject("WaterUse:RainCollector", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "FaultModel:TemperatureSensorOffset:OutdoorAir",
                new IdfObject("FaultModel:TemperatureSensorOffset:OutdoorAir", false, IdfObjectFormat.NotSpecified,
                    false, 6, false)
            },
            {
                "FaultModel:HumiditySensorOffset:OutdoorAir",
                new IdfObject("FaultModel:HumiditySensorOffset:OutdoorAir", false, IdfObjectFormat.NotSpecified, false,
                    6, false)
            },
            {
                "FaultModel:EnthalpySensorOffset:OutdoorAir",
                new IdfObject("FaultModel:EnthalpySensorOffset:OutdoorAir", false, IdfObjectFormat.NotSpecified, false,
                    6, false)
            },
            {
                "FaultModel:TemperatureSensorOffset:ReturnAir",
                new IdfObject("FaultModel:TemperatureSensorOffset:ReturnAir", false, IdfObjectFormat.NotSpecified,
                    false, 6, false)
            },
            {
                "FaultModel:EnthalpySensorOffset:ReturnAir",
                new IdfObject("FaultModel:EnthalpySensorOffset:ReturnAir", false, IdfObjectFormat.NotSpecified, false,
                    6, false)
            },
            {
                "FaultModel:TemperatureSensorOffset:ChillerSupplyWater",
                new IdfObject("FaultModel:TemperatureSensorOffset:ChillerSupplyWater", false,
                    IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:TemperatureSensorOffset:CoilSupplyAir",
                new IdfObject("FaultModel:TemperatureSensorOffset:CoilSupplyAir", false, IdfObjectFormat.NotSpecified,
                    false, 6, false)
            },
            {
                "FaultModel:TemperatureSensorOffset:CondenserSupplyWater",
                new IdfObject("FaultModel:TemperatureSensorOffset:CondenserSupplyWater", false,
                    IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:ThermostatOffset",
                new IdfObject("FaultModel:ThermostatOffset", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "FaultModel:HumidistatOffset",
                new IdfObject("FaultModel:HumidistatOffset", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:Fouling:AirFilter",
                new IdfObject("FaultModel:Fouling:AirFilter", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:Fouling:Boiler",
                new IdfObject("FaultModel:Fouling:Boiler", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:Fouling:EvaporativeCooler",
                new IdfObject("FaultModel:Fouling:EvaporativeCooler", false, IdfObjectFormat.NotSpecified, false, 6,
                    false)
            },
            {
                "FaultModel:Fouling:Chiller",
                new IdfObject("FaultModel:Fouling:Chiller", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:Fouling:CoolingTower",
                new IdfObject("FaultModel:Fouling:CoolingTower", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "FaultModel:Fouling:Coil",
                new IdfObject("FaultModel:Fouling:Coil", false, IdfObjectFormat.NotSpecified, false, 6, false)
            },
            {
                "Matrix:TwoDimension",
                new IdfObject("Matrix:TwoDimension", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "HybridModel:Zone",
                new IdfObject("HybridModel:Zone", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Curve:Linear", new IdfObject("Curve:Linear", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Curve:QuadLinear",
                new IdfObject("Curve:QuadLinear", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:Quadratic",
                new IdfObject("Curve:Quadratic", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Curve:Cubic", new IdfObject("Curve:Cubic", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Curve:Quartic", new IdfObject("Curve:Quartic", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Curve:Exponent", new IdfObject("Curve:Exponent", false, IdfObjectFormat.NotSpecified, false, 6, false)},
            {"Curve:Bicubic", new IdfObject("Curve:Bicubic", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Curve:Biquadratic",
                new IdfObject("Curve:Biquadratic", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:QuadraticLinear",
                new IdfObject("Curve:QuadraticLinear", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:CubicLinear",
                new IdfObject("Curve:CubicLinear", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:Triquadratic",
                new IdfObject("Curve:Triquadratic", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:Functional:PressureDrop",
                new IdfObject("Curve:Functional:PressureDrop", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Curve:FanPressureRise",
                new IdfObject("Curve:FanPressureRise", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:ExponentialSkewNormal",
                new IdfObject("Curve:ExponentialSkewNormal", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Curve:Sigmoid", new IdfObject("Curve:Sigmoid", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Curve:RectangularHyperbola1",
                new IdfObject("Curve:RectangularHyperbola1", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:RectangularHyperbola2",
                new IdfObject("Curve:RectangularHyperbola2", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:ExponentialDecay",
                new IdfObject("Curve:ExponentialDecay", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:DoubleExponentialDecay",
                new IdfObject("Curve:DoubleExponentialDecay", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Curve:ChillerPartLoadWithLift",
                new IdfObject("Curve:ChillerPartLoadWithLift", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Table:IndependentVariable",
                new IdfObject("Table:IndependentVariable", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Table:IndependentVariableList",
                new IdfObject("Table:IndependentVariableList", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Table:Lookup", new IdfObject("Table:Lookup", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "FluidProperties:Name",
                new IdfObject("FluidProperties:Name", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "FluidProperties:GlycolConcentration",
                new IdfObject("FluidProperties:GlycolConcentration", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "FluidProperties:Temperatures",
                new IdfObject("FluidProperties:Temperatures", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "FluidProperties:Saturated",
                new IdfObject("FluidProperties:Saturated", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "FluidProperties:Superheated",
                new IdfObject("FluidProperties:Superheated", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "FluidProperties:Concentration",
                new IdfObject("FluidProperties:Concentration", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"CurrencyType", new IdfObject("CurrencyType", true, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "ComponentCost:Adjustments",
                new IdfObject("ComponentCost:Adjustments", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ComponentCost:Reference",
                new IdfObject("ComponentCost:Reference", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "ComponentCost:LineItem",
                new IdfObject("ComponentCost:LineItem", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "UtilityCost:Tariff",
                new IdfObject("UtilityCost:Tariff", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "UtilityCost:Qualify",
                new IdfObject("UtilityCost:Qualify", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "UtilityCost:Charge:Simple",
                new IdfObject("UtilityCost:Charge:Simple", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "UtilityCost:Charge:Block",
                new IdfObject("UtilityCost:Charge:Block", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "UtilityCost:Ratchet",
                new IdfObject("UtilityCost:Ratchet", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "UtilityCost:Variable",
                new IdfObject("UtilityCost:Variable", false, IdfObjectFormat.NotSpecified, false, 3, false)
            },
            {
                "UtilityCost:Computation",
                new IdfObject("UtilityCost:Computation", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "LifeCycleCost:Parameters",
                new IdfObject("LifeCycleCost:Parameters", true, IdfObjectFormat.NotSpecified, false, 11, false)
            },
            {
                "LifeCycleCost:RecurringCosts",
                new IdfObject("LifeCycleCost:RecurringCosts", false, IdfObjectFormat.NotSpecified, false, 7, false)
            },
            {
                "LifeCycleCost:NonrecurringCost",
                new IdfObject("LifeCycleCost:NonrecurringCost", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "LifeCycleCost:UsePriceEscalation",
                new IdfObject("LifeCycleCost:UsePriceEscalation", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "LifeCycleCost:UseAdjustment",
                new IdfObject("LifeCycleCost:UseAdjustment", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Parametric:SetValueForRun",
                new IdfObject("Parametric:SetValueForRun", false, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "Parametric:Logic",
                new IdfObject("Parametric:Logic", true, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "Parametric:RunControl",
                new IdfObject("Parametric:RunControl", true, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "Parametric:FileNameSuffix",
                new IdfObject("Parametric:FileNameSuffix", true, IdfObjectFormat.NotSpecified, false, 2, false)
            },
            {
                "Output:VariableDictionary",
                new IdfObject("Output:VariableDictionary", false, IdfObjectFormat.NotSpecified, false, 1, false)
            },
            {
                "Output:Surfaces:List",
                new IdfObject("Output:Surfaces:List", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Surfaces:Drawing",
                new IdfObject("Output:Surfaces:Drawing", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Schedules",
                new IdfObject("Output:Schedules", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Constructions",
                new IdfObject("Output:Constructions", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:EnergyManagementSystem",
                new IdfObject("Output:EnergyManagementSystem", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "OutputControl:SurfaceColorScheme",
                new IdfObject("OutputControl:SurfaceColorScheme", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Table:SummaryReports",
                new IdfObject("Output:Table:SummaryReports", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Table:TimeBins",
                new IdfObject("Output:Table:TimeBins", false, IdfObjectFormat.NotSpecified, false, 5, false)
            },
            {
                "Output:Table:Monthly",
                new IdfObject("Output:Table:Monthly", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Table:Annual",
                new IdfObject("Output:Table:Annual", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "OutputControl:Table:Style",
                new IdfObject("OutputControl:Table:Style", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "OutputControl:ReportingTolerances",
                new IdfObject("OutputControl:ReportingTolerances", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Variable",
                new IdfObject("Output:Variable", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Output:Meter", new IdfObject("Output:Meter", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Output:Meter:MeterFileOnly",
                new IdfObject("Output:Meter:MeterFileOnly", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Meter:Cumulative",
                new IdfObject("Output:Meter:Cumulative", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:Meter:Cumulative:MeterFileOnly",
                new IdfObject("Output:Meter:Cumulative:MeterFileOnly", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {"Meter:Custom", new IdfObject("Meter:Custom", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Meter:CustomDecrement",
                new IdfObject("Meter:CustomDecrement", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"Output:JSON", new IdfObject("Output:JSON", true, IdfObjectFormat.NotSpecified, false, -1, false)},
            {"Output:SQLite", new IdfObject("Output:SQLite", true, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Output:EnvironmentalImpactFactors",
                new IdfObject("Output:EnvironmentalImpactFactors", false, IdfObjectFormat.NotSpecified, false, -1,
                    false)
            },
            {
                "EnvironmentalImpactFactors",
                new IdfObject("EnvironmentalImpactFactors", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {"FuelFactors", new IdfObject("FuelFactors", false, IdfObjectFormat.NotSpecified, false, -1, false)},
            {
                "Output:Diagnostics",
                new IdfObject("Output:Diagnostics", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:DebuggingData",
                new IdfObject("Output:DebuggingData", true, IdfObjectFormat.NotSpecified, false, -1, false)
            },
            {
                "Output:PreprocessorMessage",
                new IdfObject("Output:PreprocessorMessage", false, IdfObjectFormat.NotSpecified, false, -1, false)
            },

        };
    }
}