# idflint

`idflint` is a linter for EnergyPlus input files. `idflint` checks the
entire input file for mistakes that would normally be caught when
running the simulation.

`idflint` was born out of the frustration of wasting time waiting for an EnergyPlus
simulation to run for minutes before a cryptic error occurs, in which a
comma in the idf file was missing. 

`idflint` aims to speed up development and debugging time. It does this
by providing instant feedback about the validity of the complete input
file without worrying about the energy calculations themselves. It is
heavily inspired by other linting tools such as [ESLint](eslint.org).

## Installation and Usage

At it's core, `idflint` is a command-line application that takes an idf
file as input and prints errors and warning to the standard output. This
allows `idflint` to be integrated into numerous workflows, pipelines, or
wrapper programs.

The simplest way to use `idflint` to call the program from a shell
(bash, cmd.exe, PowerShell, fish, etc.) like so:

```sh
idflint in.idf
```

Each error or warning will print as a single line to the standard
output with the format `{line num}:{column num} {error message}`.

