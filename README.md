# idflint

<p align="center"><img src="brand/logo.png" width="30%">  </img></p>

`idflint` is a linter for EnergyPlus input files. `idflint` checks the
entire input file for mistakes that would normally be caught when
running the simulation.

`idflint` was born out of the frustration of wasting time waiting for an EnergyPlus
simulation to run for minutes before a cryptic error occurs, in which a
comma in the idf file was missing. 

`idflint` aims to speed up development and debugging time. It does this
by providing instant feedback about the validity of the complete input
file without worrying about the energy calculations themselves. It is
heavily inspired by other linting tools such as [ESLint](https://eslint.org).

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

## Object Data

`idflint` lints against the object definitions for the EnergyPlus
version named in the file's `Version` object. The definitions are
per-version SQLite files published by
[idf-default-objects](https://github.com/mitchpaulus/idf-default-objects),
downloaded on first use (about 5 MB per version) and cached in
`~/.local/share/idf-lint` on Linux/macOS (respecting `XDG_DATA_HOME`) or
`%LocalAppData%\idf-lint` on Windows. Set `IDF_LINT_DATA_DIR` to
override the location. If a file's version has no published data, the
nearest available version is used and a warning is printed to standard
error; files with no `Version` object are linted against 24.2.0. For
offline use, place the `{version}.sqlite3` files in the data directory
manually.

