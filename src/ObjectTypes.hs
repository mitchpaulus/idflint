module ObjectTypes where

-- Used awk '/^[a-zA-Z][a-zA-Z0-9:]*,/ { gsub(/:/,"");  split($0, array, ","); print array[1]  " | " }' Energy+.idd
-- for this.
data IdfObjectType =
    Version |
    SimulationControl |
    PerformancePrecisionTradeoffs |
    Building |
    ShadowCalculation |
    SurfaceConvectionAlgorithmInside |
    SurfaceConvectionAlgorithmOutside |
    HeatBalanceAlgorithm |
    HeatBalanceSettingsConductionFiniteDifference |
    ZoneAirHeatBalanceAlgorithm |
    ZoneAirContaminantBalance |
    ZoneAirMassFlowConservation |
    ZoneCapacitanceMultiplierResearchSpecial |
    Timestep |
    ConvergenceLimits |
    HVACSystemRootFindingAlgorithm |
    ComplianceBuilding |
    SiteLocation |
    SiteVariableLocation |
    SizingPeriodDesignDay |
    SizingPeriodWeatherFileDays |
    SizingPeriodWeatherFileConditionType |
    RunPeriod |
    RunPeriodControlSpecialDays |
    RunPeriodControlDaylightSavingTime |
    WeatherPropertySkyTemperature |
    SiteWeatherStation |
    SiteHeightVariation |
    SiteGroundTemperatureBuildingSurface |
    SiteGroundTemperatureFCfactorMethod |
    SiteGroundTemperatureShallow |
    SiteGroundTemperatureDeep |
    SiteGroundTemperatureUndisturbedFiniteDifference |
    SiteGroundTemperatureUndisturbedKusudaAchenbach |
    SiteGroundTemperatureUndisturbedXing |
    SiteGroundDomainSlab |
    SiteGroundDomainBasement |
    SiteGroundReflectance |
    SiteGroundReflectanceSnowModifier |
    SiteWaterMainsTemperature |
    SitePrecipitation |
    RoofIrrigation |
    SiteSolarAndVisibleSpectrum |
    SiteSpectrumData |
    ScheduleTypeLimits |
    ScheduleDayHourly |
    ScheduleDayInterval |
    ScheduleDayList |
    ScheduleWeekDaily |
    ScheduleWeekCompact |
    ScheduleYear |
    ScheduleCompact |
    ScheduleConstant |
    ScheduleFileShading |
    ScheduleFile |
    Material |
    MaterialNoMass |
    MaterialInfraredTransparent |
    MaterialAirGap |
    MaterialRoofVegetation |
    WindowMaterialSimpleGlazingSystem |
    WindowMaterialGlazing |
    WindowMaterialGlazingGroupThermochromic |
    WindowMaterialGlazingRefractionExtinctionMethod |
    WindowMaterialGas |
    WindowGapSupportPillar |
    WindowGapDeflectionState |
    WindowMaterialGasMixture |
    WindowMaterialGap |
    WindowMaterialShade |
    WindowMaterialComplexShade |
    WindowMaterialBlind |
    WindowMaterialScreen |
    WindowMaterialShadeEquivalentLayer |
    WindowMaterialDrapeEquivalentLayer |
    WindowMaterialBlindEquivalentLayer |
    WindowMaterialScreenEquivalentLayer |
    WindowMaterialGlazingEquivalentLayer |
    WindowMaterialGapEquivalentLayer |
    MaterialPropertyMoisturePenetrationDepthSettings |
    MaterialPropertyPhaseChange |
    MaterialPropertyPhaseChangeHysteresis |
    MaterialPropertyVariableThermalConductivity |
    MaterialPropertyHeatAndMoistureTransferSettings |
    MaterialPropertyHeatAndMoistureTransferSorptionIsotherm |
    MaterialPropertyHeatAndMoistureTransferSuction |
    MaterialPropertyHeatAndMoistureTransferRedistribution |
    MaterialPropertyHeatAndMoistureTransferDiffusion |
    MaterialPropertyHeatAndMoistureTransferThermalConductivity |
    MaterialPropertyGlazingSpectralData |
    Construction |
    ConstructionCfactorUndergroundWall |
    ConstructionFfactorGroundFloor |
    ConstructionInternalSource |
    ConstructionAirBoundary |
    WindowThermalModelParams |
    WindowsCalculationEngine |
    ConstructionComplexFenestrationState |
    ConstructionWindowEquivalentLayer |
    ConstructionWindowDataFile |
    GlobalGeometryRules |
    GeometryTransform |
    Zone |
    ZoneList |
    ZoneGroup |
    BuildingSurfaceDetailed |
    WallDetailed |
    RoofCeilingDetailed |
    FloorDetailed |
    WallExterior |
    WallAdiabatic |
    WallUnderground |
    WallInterzone |
    Roof |
    CeilingAdiabatic |
    CeilingInterzone |
    FloorGroundContact |
    FloorAdiabatic |
    FloorInterzone |
    FenestrationSurfaceDetailed |
    Window |
    Door |
    GlazedDoor |
    WindowInterzone |
    DoorInterzone |
    GlazedDoorInterzone |
    WindowShadingControl |
    WindowPropertyFrameAndDivider |
    WindowPropertyAirflowControl |
    WindowPropertyStormWindow |
    InternalMass |
    ShadingSite |
    ShadingBuilding |
    ShadingSiteDetailed |
    ShadingBuildingDetailed |
    ShadingOverhang |
    ShadingOverhangProjection |
    ShadingFin |
    ShadingFinProjection |
    ShadingZoneDetailed |
    ShadingPropertyReflectance |
    SurfacePropertyHeatTransferAlgorithm |
    SurfacePropertyHeatTransferAlgorithmMultipleSurface |
    SurfacePropertyHeatTransferAlgorithmSurfaceList |
    SurfacePropertyHeatTransferAlgorithmConstruction |
    SurfacePropertyHeatBalanceSourceTerm |
    SurfaceControlMovableInsulation |
    SurfacePropertyOtherSideCoefficients |
    SurfacePropertyOtherSideConditionsModel |
    SurfacePropertyUnderwater |
    FoundationKiva |
    FoundationKivaSettings |
    SurfacePropertyExposedFoundationPerimeter |
    SurfaceConvectionAlgorithmInsideAdaptiveModelSelections |
    SurfaceConvectionAlgorithmOutsideAdaptiveModelSelections |
    SurfaceConvectionAlgorithmInsideUserCurve |
    SurfaceConvectionAlgorithmOutsideUserCurve |
    SurfacePropertyConvectionCoefficients |
    SurfacePropertyConvectionCoefficientsMultipleSurface |
    SurfacePropertiesVaporCoefficients |
    SurfacePropertyExteriorNaturalVentedCavity |
    SurfacePropertySolarIncidentInside |
    SurfacePropertyLocalEnvironment |
    ZonePropertyLocalEnvironment |
    SurfacePropertySurroundingSurfaces |
    ComplexFenestrationPropertySolarAbsorbedLayers |
    ZonePropertyUserViewFactorsbySurfaceName |
    GroundHeatTransferControl |
    GroundHeatTransferSlabMaterials |
    GroundHeatTransferSlabMatlProps |
    GroundHeatTransferSlabBoundConds |
    GroundHeatTransferSlabBldgProps |
    GroundHeatTransferSlabInsulation |
    GroundHeatTransferSlabEquivalentSlab |
    GroundHeatTransferSlabAutoGrid |
    GroundHeatTransferSlabManualGrid |
    GroundHeatTransferSlabXFACE |
    GroundHeatTransferSlabYFACE |
    GroundHeatTransferSlabZFACE |
    GroundHeatTransferBasementSimParameters |
    GroundHeatTransferBasementMatlProps |
    GroundHeatTransferBasementInsulation |
    GroundHeatTransferBasementSurfaceProps |
    GroundHeatTransferBasementBldgData |
    GroundHeatTransferBasementInterior |
    GroundHeatTransferBasementComBldg |
    GroundHeatTransferBasementEquivSlab |
    GroundHeatTransferBasementEquivAutoGrid |
    GroundHeatTransferBasementAutoGrid |
    GroundHeatTransferBasementManualGrid |
    GroundHeatTransferBasementXFACE |
    GroundHeatTransferBasementYFACE |
    GroundHeatTransferBasementZFACE |
    RoomAirModelType |
    RoomAirTemperaturePatternUserDefined |
    RoomAirTemperaturePatternConstantGradient |
    RoomAirTemperaturePatternTwoGradient |
    RoomAirTemperaturePatternNondimensionalHeight |
    RoomAirTemperaturePatternSurfaceMapping |
    RoomAirNode |
    RoomAirSettingsOneNodeDisplacementVentilation |
    RoomAirSettingsThreeNodeDisplacementVentilation |
    RoomAirSettingsCrossVentilation |
    RoomAirSettingsUnderFloorAirDistributionInterior |
    RoomAirSettingsUnderFloorAirDistributionExterior |
    RoomAirNodeAirflowNetwork |
    RoomAirNodeAirflowNetworkInternalGains |
    RoomAirNodeAirflowNetworkHVACEquipment |
    RoomAirSettingsAirflowNetwork |
    People |
    ComfortViewFactorAngles |
    Lights |
    ElectricEquipment |
    GasEquipment |
    HotWaterEquipment |
    SteamEquipment |
    OtherEquipment |
    ElectricEquipmentITEAirCooled |
    ZoneBaseboardOutdoorTemperatureControlled |
    SwimmingPoolIndoor |
    ZoneContaminantSourceAndSinkCarbonDioxide |
    ZoneContaminantSourceAndSinkGenericConstant |
    SurfaceContaminantSourceAndSinkGenericPressureDriven |
    ZoneContaminantSourceAndSinkGenericCutoffModel |
    ZoneContaminantSourceAndSinkGenericDecaySource |
    SurfaceContaminantSourceAndSinkGenericBoundaryLayerDiffusion |
    SurfaceContaminantSourceAndSinkGenericDepositionVelocitySink |
    ZoneContaminantSourceAndSinkGenericDepositionRateSink |
    DaylightingControls |
    DaylightingReferencePoint |
    DaylightingDELightComplexFenestration |
    DaylightingDeviceTubular |
    DaylightingDeviceShelf |
    DaylightingDeviceLightWell |
    OutputDaylightFactors |
    OutputIlluminanceMap |
    OutputControlIlluminanceMapStyle |
    ZoneInfiltrationDesignFlowRate |
    ZoneInfiltrationEffectiveLeakageArea |
    ZoneInfiltrationFlowCoefficient |
    ZoneVentilationDesignFlowRate |
    ZoneVentilationWindandStackOpenArea |
    ZoneAirBalanceOutdoorAir |
    ZoneMixing |
    ZoneCrossMixing |
    ZoneRefrigerationDoorMixing |
    ZoneEarthtube |
    ZoneCoolTowerShower |
    ZoneThermalChimney |
    AirflowNetworkSimulationControl |
    AirflowNetworkMultiZoneZone |
    AirflowNetworkMultiZoneSurface |
    AirflowNetworkMultiZoneReferenceCrackConditions |
    AirflowNetworkMultiZoneSurfaceCrack |
    AirflowNetworkMultiZoneSurfaceEffectiveLeakageArea |
    AirflowNetworkMultiZoneComponentDetailedOpening |
    AirflowNetworkMultiZoneComponentSimpleOpening |
    AirflowNetworkMultiZoneComponentHorizontalOpening |
    AirflowNetworkMultiZoneComponentZoneExhaustFan |
    AirflowNetworkMultiZoneExternalNode |
    AirflowNetworkMultiZoneWindPressureCoefficientArray |
    AirflowNetworkMultiZoneWindPressureCoefficientValues |
    AirflowNetworkZoneControlPressureController |
    AirflowNetworkDistributionNode |
    AirflowNetworkDistributionComponentLeak |
    AirflowNetworkDistributionComponentLeakageRatio |
    AirflowNetworkDistributionComponentDuct |
    AirflowNetworkDistributionComponentFan |
    AirflowNetworkDistributionComponentCoil |
    AirflowNetworkDistributionComponentHeatExchanger |
    AirflowNetworkDistributionComponentTerminalUnit |
    AirflowNetworkDistributionComponentConstantPressureDrop |
    AirflowNetworkDistributionComponentOutdoorAirFlow |
    AirflowNetworkDistributionComponentReliefAirFlow |
    AirflowNetworkDistributionLinkage |
    AirflowNetworkDistributionDuctViewFactors |
    AirflowNetworkOccupantVentilationControl |
    AirflowNetworkIntraZoneNode |
    AirflowNetworkIntraZoneLinkage |
    ExteriorLights |
    ExteriorFuelEquipment |
    ExteriorWaterEquipment |
    HVACTemplateThermostat |
    HVACTemplateZoneIdealLoadsAirSystem |
    HVACTemplateZoneBaseboardHeat |
    HVACTemplateZoneFanCoil |
    HVACTemplateZonePTAC |
    HVACTemplateZonePTHP |
    HVACTemplateZoneWaterToAirHeatPump |
    HVACTemplateZoneVRF |
    HVACTemplateZoneUnitary |
    HVACTemplateZoneVAV |
    HVACTemplateZoneVAVFanPowered |
    HVACTemplateZoneVAVHeatAndCool |
    HVACTemplateZoneConstantVolume |
    HVACTemplateZoneDualDuct |
    HVACTemplateSystemVRF |
    HVACTemplateSystemUnitary |
    HVACTemplateSystemUnitaryHeatPumpAirToAir |
    HVACTemplateSystemUnitarySystem |
    HVACTemplateSystemVAV |
    HVACTemplateSystemPackagedVAV |
    HVACTemplateSystemConstantVolume |
    HVACTemplateSystemDualDuct |
    HVACTemplateSystemDedicatedOutdoorAir |
    HVACTemplatePlantChilledWaterLoop |
    HVACTemplatePlantChiller |
    HVACTemplatePlantChillerObjectReference |
    HVACTemplatePlantTower |
    HVACTemplatePlantTowerObjectReference |
    HVACTemplatePlantHotWaterLoop |
    HVACTemplatePlantBoiler |
    HVACTemplatePlantBoilerObjectReference |
    HVACTemplatePlantMixedWaterLoop |
    DesignSpecificationOutdoorAir |
    DesignSpecificationZoneAirDistribution |
    SizingParameters |
    SizingZone |
    DesignSpecificationZoneHVACSizing |
    DesignSpecificationAirTerminalSizing |
    SizingSystem |
    SizingPlant |
    OutputControlSizingStyle |
    ZoneControlHumidistat |
    ZoneControlThermostat |
    ZoneControlThermostatOperativeTemperature |
    ZoneControlThermostatThermalComfort |
    ZoneControlThermostatTemperatureAndHumidity |
    ThermostatSetpointSingleHeating |
    ThermostatSetpointSingleCooling |
    ThermostatSetpointSingleHeatingOrCooling |
    ThermostatSetpointDualSetpoint |
    ThermostatSetpointThermalComfortFangerSingleHeating |
    ThermostatSetpointThermalComfortFangerSingleCooling |
    ThermostatSetpointThermalComfortFangerSingleHeatingOrCooling |
    ThermostatSetpointThermalComfortFangerDualSetpoint |
    ZoneControlThermostatStagedDualSetpoint |
    ZoneControlContaminantController |
    ZoneHVACIdealLoadsAirSystem |
    ZoneHVACFourPipeFanCoil |
    ZoneHVACWindowAirConditioner |
    ZoneHVACPackagedTerminalAirConditioner |
    ZoneHVACPackagedTerminalHeatPump |
    ZoneHVACWaterToAirHeatPump |
    ZoneHVACDehumidifierDX |
    ZoneHVACEnergyRecoveryVentilator |
    ZoneHVACEnergyRecoveryVentilatorController |
    ZoneHVACUnitVentilator |
    ZoneHVACUnitHeater |
    ZoneHVACEvaporativeCoolerUnit |
    ZoneHVACHybridUnitaryHVAC |
    ZoneHVACOutdoorAirUnit |
    ZoneHVACOutdoorAirUnitEquipmentList |
    ZoneHVACTerminalUnitVariableRefrigerantFlow |
    ZoneHVACBaseboardRadiantConvectiveWater |
    ZoneHVACBaseboardRadiantConvectiveSteam |
    ZoneHVACBaseboardRadiantConvectiveElectric |
    ZoneHVACCoolingPanelRadiantConvectiveWater |
    ZoneHVACBaseboardConvectiveWater |
    ZoneHVACBaseboardConvectiveElectric |
    ZoneHVACLowTemperatureRadiantVariableFlow |
    ZoneHVACLowTemperatureRadiantConstantFlow |
    ZoneHVACLowTemperatureRadiantElectric |
    ZoneHVACLowTemperatureRadiantSurfaceGroup |
    ZoneHVACHighTemperatureRadiant |
    ZoneHVACVentilatedSlab |
    ZoneHVACVentilatedSlabSlabGroup |
    AirTerminalSingleDuctUncontrolled |
    AirTerminalSingleDuctConstantVolumeReheat |
    AirTerminalSingleDuctConstantVolumeNoReheat |
    AirTerminalSingleDuctVAVNoReheat |
    AirTerminalSingleDuctVAVReheat |
    AirTerminalSingleDuctVAVReheatVariableSpeedFan |
    AirTerminalSingleDuctVAVHeatAndCoolNoReheat |
    AirTerminalSingleDuctVAVHeatAndCoolReheat |
    AirTerminalSingleDuctSeriesPIUReheat |
    AirTerminalSingleDuctParallelPIUReheat |
    AirTerminalSingleDuctConstantVolumeFourPipeInduction |
    AirTerminalSingleDuctConstantVolumeFourPipeBeam |
    AirTerminalSingleDuctConstantVolumeCooledBeam |
    AirTerminalSingleDuctMixer |
    AirTerminalDualDuctConstantVolume |
    AirTerminalDualDuctVAV |
    AirTerminalDualDuctVAVOutdoorAir |
    ZoneHVACAirDistributionUnit |
    ZoneHVACEquipmentList |
    ZoneHVACEquipmentConnections |
    FanSystemModel |
    FanConstantVolume |
    FanVariableVolume |
    FanOnOff |
    FanZoneExhaust |
    FanPerformanceNightVentilation |
    FanComponentModel |
    CoilCoolingWater |
    CoilCoolingWaterDetailedGeometry |
    CoilCoolingDXSingleSpeed |
    CoilCoolingDXTwoSpeed |
    CoilCoolingDXMultiSpeed |
    CoilCoolingDXVariableSpeed |
    CoilCoolingDXTwoStageWithHumidityControlMode |
    CoilPerformanceDXCooling |
    CoilCoolingDXVariableRefrigerantFlow |
    CoilHeatingDXVariableRefrigerantFlow |
    CoilCoolingDXVariableRefrigerantFlowFluidTemperatureControl |
    CoilHeatingDXVariableRefrigerantFlowFluidTemperatureControl |
    CoilHeatingWater |
    CoilHeatingSteam |
    CoilHeatingElectric |
    CoilHeatingElectricMultiStage |
    CoilHeatingFuel |
    CoilHeatingGasMultiStage |
    CoilHeatingDesuperheater |
    CoilHeatingDXSingleSpeed |
    CoilHeatingDXMultiSpeed |
    CoilHeatingDXVariableSpeed |
    CoilCoolingWaterToAirHeatPumpParameterEstimation |
    CoilHeatingWaterToAirHeatPumpParameterEstimation |
    CoilCoolingWaterToAirHeatPumpEquationFit |
    CoilCoolingWaterToAirHeatPumpVariableSpeedEquationFit |
    CoilHeatingWaterToAirHeatPumpEquationFit |
    CoilHeatingWaterToAirHeatPumpVariableSpeedEquationFit |
    CoilWaterHeatingAirToWaterHeatPumpPumped |
    CoilWaterHeatingAirToWaterHeatPumpWrapped |
    CoilWaterHeatingAirToWaterHeatPumpVariableSpeed |
    CoilWaterHeatingDesuperheater |
    CoilSystemCoolingDX |
    CoilSystemHeatingDX |
    CoilSystemCoolingWaterHeatExchangerAssisted |
    CoilSystemCoolingDXHeatExchangerAssisted |
    CoilSystemIntegratedHeatPumpAirSource |
    CoilCoolingDXSingleSpeedThermalStorage |
    EvaporativeCoolerDirectCelDekPad |
    EvaporativeCoolerIndirectCelDekPad |
    EvaporativeCoolerIndirectWetCoil |
    EvaporativeCoolerIndirectResearchSpecial |
    EvaporativeCoolerDirectResearchSpecial |
    HumidifierSteamElectric |
    HumidifierSteamGas |
    DehumidifierDesiccantNoFans |
    DehumidifierDesiccantSystem |
    HeatExchangerAirToAirFlatPlate |
    HeatExchangerAirToAirSensibleAndLatent |
    HeatExchangerDesiccantBalancedFlow |
    HeatExchangerDesiccantBalancedFlowPerformanceDataType1 |
    AirLoopHVACUnitarySystem |
    UnitarySystemPerformanceMultispeed |
    AirLoopHVACUnitaryFurnaceHeatOnly |
    AirLoopHVACUnitaryFurnaceHeatCool |
    AirLoopHVACUnitaryHeatOnly |
    AirLoopHVACUnitaryHeatCool |
    AirLoopHVACUnitaryHeatPumpAirToAir |
    AirLoopHVACUnitaryHeatPumpWaterToAir |
    AirLoopHVACUnitaryHeatCoolVAVChangeoverBypass |
    AirLoopHVACUnitaryHeatPumpAirToAirMultiSpeed |
    AirConditionerVariableRefrigerantFlow |
    AirConditionerVariableRefrigerantFlowFluidTemperatureControl |
    AirConditionerVariableRefrigerantFlowFluidTemperatureControlHR |
    ZoneTerminalUnitList |
    ControllerWaterCoil |
    ControllerOutdoorAir |
    ControllerMechanicalVentilation |
    AirLoopHVACControllerList |
    AirLoopHVAC |
    AirLoopHVACOutdoorAirSystemEquipmentList |
    AirLoopHVACOutdoorAirSystem |
    OutdoorAirMixer |
    AirLoopHVACZoneSplitter |
    AirLoopHVACSupplyPlenum |
    AirLoopHVACSupplyPath |
    AirLoopHVACZoneMixer |
    AirLoopHVACReturnPlenum |
    AirLoopHVACReturnPath |
    AirLoopHVACDedicatedOutdoorAirSystem |
    AirLoopHVACMixer |
    AirLoopHVACSplitter |
    Branch |
    BranchList |
    ConnectorSplitter |
    ConnectorMixer |
    ConnectorList |
    NodeList |
    OutdoorAirNode |
    OutdoorAirNodeList |
    PipeAdiabatic |
    PipeAdiabaticSteam |
    PipeIndoor |
    PipeOutdoor |
    PipeUnderground |
    PipingSystemUndergroundDomain |
    PipingSystemUndergroundPipeCircuit |
    PipingSystemUndergroundPipeSegment |
    Duct |
    PumpVariableSpeed |
    PumpConstantSpeed |
    PumpVariableSpeedCondensate |
    HeaderedPumpsConstantSpeed |
    HeaderedPumpsVariableSpeed |
    TemperingValve |
    LoadProfilePlant |
    SolarCollectorPerformanceFlatPlate |
    SolarCollectorFlatPlateWater |
    SolarCollectorFlatPlatePhotovoltaicThermal |
    SolarCollectorPerformancePhotovoltaicThermalSimple |
    SolarCollectorIntegralCollectorStorage |
    SolarCollectorPerformanceIntegralCollectorStorage |
    SolarCollectorUnglazedTranspired |
    SolarCollectorUnglazedTranspiredMultisystem |
    BoilerHotWater |
    BoilerSteam |
    ChillerElectricEIR |
    ChillerElectricReformulatedEIR |
    ChillerElectric |
    ChillerAbsorptionIndirect |
    ChillerAbsorption |
    ChillerConstantCOP |
    ChillerEngineDriven |
    ChillerCombustionTurbine |
    ChillerHeaterAbsorptionDirectFired |
    ChillerHeaterAbsorptionDoubleEffect |
    HeatPumpWaterToWaterEIRCooling |
    HeatPumpWaterToWaterEIRHeating |
    HeatPumpWaterToWaterEquationFitHeating |
    HeatPumpWaterToWaterEquationFitCooling |
    HeatPumpWaterToWaterParameterEstimationCooling |
    HeatPumpWaterToWaterParameterEstimationHeating |
    DistrictCooling |
    DistrictHeating |
    PlantComponentTemperatureSource |
    CentralHeatPumpSystem |
    ChillerHeaterPerformanceElectricEIR |
    CoolingTowerSingleSpeed |
    CoolingTowerTwoSpeed |
    CoolingTowerVariableSpeedMerkel |
    CoolingTowerVariableSpeed |
    CoolingTowerPerformanceCoolTools |
    CoolingTowerPerformanceYorkCalc |
    EvaporativeFluidCoolerSingleSpeed |
    EvaporativeFluidCoolerTwoSpeed |
    FluidCoolerSingleSpeed |
    FluidCoolerTwoSpeed |
    GroundHeatExchangerSystem |
    GroundHeatExchangerVerticalProperties |
    GroundHeatExchangerVerticalArray |
    GroundHeatExchangerVerticalSingle |
    GroundHeatExchangerResponseFactors |
    GroundHeatExchangerPond |
    GroundHeatExchangerSurface |
    GroundHeatExchangerHorizontalTrench |
    GroundHeatExchangerSlinky |
    HeatExchangerFluidToFluid |
    WaterHeaterMixed |
    WaterHeaterStratified |
    WaterHeaterSizing |
    WaterHeaterHeatPumpPumpedCondenser |
    WaterHeaterHeatPumpWrappedCondenser |
    ThermalStorageIceSimple |
    ThermalStorageIceDetailed |
    ThermalStorageChilledWaterMixed |
    ThermalStorageChilledWaterStratified |
    PlantLoop |
    CondenserLoop |
    PlantEquipmentList |
    CondenserEquipmentList |
    PlantEquipmentOperationUncontrolled |
    PlantEquipmentOperationCoolingLoad |
    PlantEquipmentOperationHeatingLoad |
    PlantEquipmentOperationOutdoorDryBulb |
    PlantEquipmentOperationOutdoorWetBulb |
    PlantEquipmentOperationOutdoorRelativeHumidity |
    PlantEquipmentOperationOutdoorDewpoint |
    PlantEquipmentOperationComponentSetpoint |
    PlantEquipmentOperationThermalEnergyStorage |
    PlantEquipmentOperationOutdoorDryBulbDifference |
    PlantEquipmentOperationOutdoorWetBulbDifference |
    PlantEquipmentOperationOutdoorDewpointDifference |
    PlantEquipmentOperationSchemes |
    CondenserEquipmentOperationSchemes |
    EnergyManagementSystemSensor |
    EnergyManagementSystemActuator |
    EnergyManagementSystemProgramCallingManager |
    EnergyManagementSystemProgram |
    EnergyManagementSystemSubroutine |
    EnergyManagementSystemGlobalVariable |
    EnergyManagementSystemOutputVariable |
    EnergyManagementSystemMeteredOutputVariable |
    EnergyManagementSystemTrendVariable |
    EnergyManagementSystemInternalVariable |
    EnergyManagementSystemCurveOrTableIndexVariable |
    EnergyManagementSystemConstructionIndexVariable |
    ExternalInterface |
    ExternalInterfaceSchedule |
    ExternalInterfaceVariable |
    ExternalInterfaceActuator |
    ExternalInterfaceFunctionalMockupUnitImport |
    ExternalInterfaceFunctionalMockupUnitImportFromVariable |
    ExternalInterfaceFunctionalMockupUnitImportToSchedule |
    ExternalInterfaceFunctionalMockupUnitImportToActuator |
    ExternalInterfaceFunctionalMockupUnitImportToVariable |
    ExternalInterfaceFunctionalMockupUnitExportFromVariable |
    ExternalInterfaceFunctionalMockupUnitExportToSchedule |
    ExternalInterfaceFunctionalMockupUnitExportToActuator |
    ExternalInterfaceFunctionalMockupUnitExportToVariable |
    ZoneHVACForcedAirUserDefined |
    PlantEquipmentOperationUserDefined |
    AvailabilityManagerScheduled |
    AvailabilityManagerScheduledOn |
    AvailabilityManagerScheduledOff |
    AvailabilityManagerOptimumStart |
    AvailabilityManagerNightCycle |
    AvailabilityManagerDifferentialThermostat |
    AvailabilityManagerHighTemperatureTurnOff |
    AvailabilityManagerHighTemperatureTurnOn |
    AvailabilityManagerLowTemperatureTurnOff |
    AvailabilityManagerLowTemperatureTurnOn |
    AvailabilityManagerNightVentilation |
    AvailabilityManagerHybridVentilation |
    AvailabilityManagerAssignmentList |
    SetpointManagerScheduled |
    SetpointManagerScheduledDualSetpoint |
    SetpointManagerOutdoorAirReset |
    SetpointManagerSingleZoneReheat |
    SetpointManagerSingleZoneHeating |
    SetpointManagerSingleZoneCooling |
    SetpointManagerSingleZoneHumidityMinimum |
    SetpointManagerSingleZoneHumidityMaximum |
    SetpointManagerMixedAir |
    SetpointManagerOutdoorAirPretreat |
    SetpointManagerWarmest |
    SetpointManagerColdest |
    SetpointManagerReturnAirBypassFlow |
    SetpointManagerWarmestTemperatureFlow |
    SetpointManagerMultiZoneHeatingAverage |
    SetpointManagerMultiZoneCoolingAverage |
    SetpointManagerMultiZoneMinimumHumidityAverage |
    SetpointManagerMultiZoneMaximumHumidityAverage |
    SetpointManagerMultiZoneHumidityMinimum |
    SetpointManagerMultiZoneHumidityMaximum |
    SetpointManagerFollowOutdoorAirTemperature |
    SetpointManagerFollowSystemNodeTemperature |
    SetpointManagerFollowGroundTemperature |
    SetpointManagerCondenserEnteringReset |
    SetpointManagerCondenserEnteringResetIdeal |
    SetpointManagerSingleZoneOneStageCooling |
    SetpointManagerSingleZoneOneStageHeating |
    SetpointManagerReturnTemperatureChilledWater |
    SetpointManagerReturnTemperatureHotWater |
    RefrigerationCase |
    RefrigerationCompressorRack |
    RefrigerationCaseAndWalkInList |
    RefrigerationCondenserAirCooled |
    RefrigerationCondenserEvaporativeCooled |
    RefrigerationCondenserWaterCooled |
    RefrigerationCondenserCascade |
    RefrigerationGasCoolerAirCooled |
    RefrigerationTransferLoadList |
    RefrigerationSubcooler |
    RefrigerationCompressor |
    RefrigerationCompressorList |
    RefrigerationSystem |
    RefrigerationTranscriticalSystem |
    RefrigerationSecondarySystem |
    RefrigerationAirChiller |
    ZoneHVACRefrigerationChillerSet |
    DemandManagerAssignmentList |
    DemandManagerExteriorLights |
    DemandManagerLights |
    DemandManagerElectricEquipment |
    DemandManagerThermostats |
    GeneratorInternalCombustionEngine |
    GeneratorCombustionTurbine |
    GeneratorMicroTurbine |
    GeneratorPhotovoltaic |
    PhotovoltaicPerformanceSimple |
    PhotovoltaicPerformanceSandia |
    GeneratorPVWatts |
    ElectricLoadCenterInverterPVWatts |
    GeneratorFuelCell |
    GeneratorFuelCellPowerModule |
    GeneratorFuelCellAirSupply |
    GeneratorFuelCellWaterSupply |
    GeneratorFuelCellAuxiliaryHeater |
    GeneratorFuelCellExhaustGasToWaterHeatExchanger |
    GeneratorFuelCellElectricalStorage |
    GeneratorFuelCellInverter |
    GeneratorMicroCHP |
    GeneratorMicroCHPNonNormalizedParameters |
    GeneratorFuelSupply |
    GeneratorWindTurbine |
    ElectricLoadCenterGenerators |
    ElectricLoadCenterInverterSimple |
    ElectricLoadCenterInverterFunctionOfPower |
    ElectricLoadCenterInverterLookUpTable |
    ElectricLoadCenterStorageSimple |
    ElectricLoadCenterStorageBattery |
    ElectricLoadCenterTransformer |
    ElectricLoadCenterDistribution |
    WaterUseEquipment |
    WaterUseConnections |
    WaterUseStorage |
    WaterUseWell |
    WaterUseRainCollector |
    FaultModelTemperatureSensorOffsetOutdoorAir |
    FaultModelHumiditySensorOffsetOutdoorAir |
    FaultModelEnthalpySensorOffsetOutdoorAir |
    FaultModelTemperatureSensorOffsetReturnAir |
    FaultModelEnthalpySensorOffsetReturnAir |
    FaultModelTemperatureSensorOffsetChillerSupplyWater |
    FaultModelTemperatureSensorOffsetCoilSupplyAir |
    FaultModelTemperatureSensorOffsetCondenserSupplyWater |
    FaultModelThermostatOffset |
    FaultModelHumidistatOffset |
    FaultModelFoulingAirFilter |
    FaultModelFoulingBoiler |
    FaultModelFoulingEvaporativeCooler |
    FaultModelFoulingChiller |
    FaultModelFoulingCoolingTower |
    FaultModelFoulingCoil |
    MatrixTwoDimension |
    HybridModelZone |
    CurveLinear |
    CurveQuadLinear |
    CurveQuadratic |
    CurveCubic |
    CurveQuartic |
    CurveExponent |
    CurveBicubic |
    CurveBiquadratic |
    CurveQuadraticLinear |
    CurveCubicLinear |
    CurveTriquadratic |
    CurveFunctionalPressureDrop |
    CurveFanPressureRise |
    CurveExponentialSkewNormal |
    CurveSigmoid |
    CurveRectangularHyperbola1 |
    CurveRectangularHyperbola2 |
    CurveExponentialDecay |
    CurveDoubleExponentialDecay |
    CurveChillerPartLoadWithLift |
    TableIndependentVariable |
    TableIndependentVariableList |
    TableLookup |
    FluidPropertiesName |
    FluidPropertiesGlycolConcentration |
    FluidPropertiesTemperatures |
    FluidPropertiesSaturated |
    FluidPropertiesSuperheated |
    FluidPropertiesConcentration |
    CurrencyType |
    ComponentCostAdjustments |
    ComponentCostReference |
    ComponentCostLineItem |
    UtilityCostTariff |
    UtilityCostQualify |
    UtilityCostChargeSimple |
    UtilityCostChargeBlock |
    UtilityCostRatchet |
    UtilityCostVariable |
    UtilityCostComputation |
    LifeCycleCostParameters |
    LifeCycleCostRecurringCosts |
    LifeCycleCostNonrecurringCost |
    LifeCycleCostUsePriceEscalation |
    LifeCycleCostUseAdjustment |
    ParametricSetValueForRun |
    ParametricLogic |
    ParametricRunControl |
    ParametricFileNameSuffix |
    OutputVariableDictionary |
    OutputSurfacesList |
    OutputSurfacesDrawing |
    OutputSchedules |
    OutputConstructions |
    OutputEnergyManagementSystem |
    OutputControlSurfaceColorScheme |
    OutputTableSummaryReports |
    OutputTableTimeBins |
    OutputTableMonthly |
    OutputTableAnnual |
    OutputControlTableStyle |
    OutputControlReportingTolerances |
    OutputVariable |
    OutputMeter |
    OutputMeterMeterFileOnly |
    OutputMeterCumulative |
    OutputMeterCumulativeMeterFileOnly |
    MeterCustom |
    MeterCustomDecrement |
    OutputJSON |
    OutputSQLite |
    OutputEnvironmentalImpactFactors |
    EnvironmentalImpactFactors |
    FuelFactors |
    OutputDiagnostics |
    OutputDebuggingData |
    OutputPreprocessorMessage