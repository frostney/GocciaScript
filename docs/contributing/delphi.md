# Delphi Validation

*How contributors build and validate GocciaScript with the supported Delphi IDE.*

## Executive Summary

- **Both architectures** — Validate Win32 and Win64 independently; neither substitutes for the other.
- **Complete application matrix** — Build and run every project in `GocciaScript.Delphi.groupproj` and run the applicable local tests.
- **Shared semantics** — Delphi adaptations belong at compiler, RTL, project, or ABI boundaries, not in separate engine semantics.
- **Supported IDE** — The current local validation target is Delphi 12 Community Edition.

## Project Group

Open `source/app/GocciaScript.Delphi.groupproj` in Delphi 12. The group owns the
REPL, both script loaders, Sandbox Runner, Test Runner, Benchmark Runner, and
Bundler. Shared search paths, compiler definitions, and output directories live
in `source/app/GocciaScript.Delphi.props`; machine-specific IDE state does not
belong in the repository.

Select `Win32` or `Win64` for the group and use **Build All Projects**. Outputs
go to `build/delphi/<platform>/<configuration>`. Each project has its own DCU
directory so changing project or architecture cannot reuse incompatible units.
The shared project properties also give each executable an 8 MiB maximum stack,
matching the native-stack assumption behind the interpreter's guarded Pascal
recursion. This is executable metadata, not a separate Delphi runtime path.

## Validation Contract

Delphi parity for a supported target requires all of the following:

1. Every project in the Delphi group compiles for the target.
2. Every produced executable starts and completes its applicable smoke run.
3. The ordinary Pascal and JavaScript tests available through the native test
   applications run and pass with the same semantics as the FPC build.
4. The process is repeated independently for Win32 and Win64.

Clearing the current IDE compiler error, building one representative program,
or validating one architecture is not parity. External compliance harnesses
that require their own pinned suite checkout and update workflow are outside the
native application matrix; ordinary repository tests remain required.

## Why Validation Is IDE-Based

The supported Delphi 12 Community Edition workflow is IDE-based. Community
Edition does not provide the command-line automation path needed to reproduce
the complete group build and test contract in ordinary per-commit CI. This is
an operational constraint on automation, not a narrower runtime or application
support tier.

## Portability Boundaries

Shared source uses the project CPU symbols normalized by `Shared.inc` and the
global Intel assembler mode established there. Platform behavior uses the
compiler-provided `MSWINDOWS` and `UNIX` families. Delphi-specific code may
adapt syntax, RTL names, project metadata, and genuine ABI differences, but it
must not select different ECMAScript text or Number semantics.

In-memory ECMAScript text remains Pascal `string`, with UTF-8 or host-native
bytes handled only by explicit boundary adapters. ICU declarations use `string`
on the Pascal side and convert to ICU's UTF-16 pointer and length representation
at the FFI call boundary.
