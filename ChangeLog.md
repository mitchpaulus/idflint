# Changelog for idf-lint

## Unreleased changes

- Warn when no `Timestep` object is found, since EnergyPlus defaults to 4 timesteps per hour ([#21](https://github.com/mitchpaulus/idflint/issues/21)).
- Error when a zone has a `ZoneHVAC:EquipmentConnections` object but no thermostat (`ZoneControl:Thermostat` or `ZoneControl:Thermostat:StagedDualSetpoint`, directly or through a `ZoneList`) associated with it ([#12](https://github.com/mitchpaulus/idflint/issues/12)).
