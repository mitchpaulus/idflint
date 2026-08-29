# Changelog for idf-lint

## Unreleased changes

- Warn when no `Timestep` object is found, since EnergyPlus defaults to 4 timesteps per hour ([#21](https://github.com/mitchpaulus/idflint/issues/21)).
- Error when a loop declared as Cooling/Condenser in `Sizing:Plant` uses a `PlantEquipmentOperation:HeatingLoad` scheme, or a Heating/Steam loop uses `PlantEquipmentOperation:CoolingLoad` ([#9](https://github.com/mitchpaulus/idflint/issues/9)).
