# 1. Enforce minimum number of fields

Some objects have a specified minimum number of fields in the IDD. The
rules enforces that corresponding objects have the minimum number of
fields.

Example of *incorrect* code:

```
ZoneAirMassFlowConservation,
    No;
```

The object `ZoneAirMassFlowConservation` has the following defintion in
the IDD. Note the `\min-fields 3` definition.

```
ZoneAirMassFlowConservation,
       \memo Enforces the zone air mass flow balance by adjusting zone mixing object and/or
       \memo infiltration object mass flow rates.
       \memo If either mixing or infiltration is active, then the zone air mass flow
       \memo balance calculation will attempt to enforce conservation of mass for each zone.
       \memo If mixing is "No" and infiltration is "None", then the zone air mass flow
       \memo calculation defaults to assume self-balanced simple flow mixing and infiltration objects.
       \unique-object
       \min-fields 3
  A1,  \field Adjust Zone Mixing For Zone Air Mass Flow Balance
       \note If Yes, Zone mixing object flow rates are adjusted to balance the zone air mass flow
       \note and additional infiltration air flow may be added if required in order to balance the
       \note zone air mass flow.
       \type choice
       \key Yes
       \key No
       \default No
  A2,  \field Infiltration Balancing Method
       \note This input field allows user to choose how zone infiltration flow is treated during
       \note the zone air mass flow balance calculation.
       \type choice
       \key AddInfiltrationFlow
       \key AdjustInfiltrationFlow
       \key None
       \default AddInfiltrationFlow
       \note AddInfiltrationFlow may add infiltration to the base flow specified in the
       \note infiltration object to balance the zone air mass flow. The additional infiltration
       \note air mass flow is not self-balanced. The base flow is assumed to be self-balanced.
       \note AdjustInfiltrationFlow may adjust the base flow calculated using
       \note the base flow specified in the infiltration object to balance the zone air mass flow. If it
       \note If no adjustment is required, then the base infiltration is assumed to be self-balanced.
       \note None will make no changes to the base infiltration flow.
  A3;  \field Infiltration Balancing Zones
       \note This input field allows user to choose which zones are included in infiltration balancing.
       \note MixingSourceZonesOnly allows infiltration balancing only in zones which as source zones for mixing
       \note which also have an infiltration object defined.
       \note AllZones allows infiltration balancing in any zone which has an infiltration object defined.
       \type choice
       \key MixingSourceZonesOnly
       \key AllZones
       \default MixingSourceZonesOnly

```
