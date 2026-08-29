# Changelog for idf-lint

## Unreleased changes

- Warn when no `Timestep` object is found, since EnergyPlus defaults to 4 timesteps per hour ([#21](https://github.com/mitchpaulus/idflint/issues/21)).
- Error when a `PlantLoop` or `CondenserLoop` has a Minimum Loop Temperature greater than its Maximum Loop Temperature ([#5](https://github.com/mitchpaulus/idflint/issues/5)).
