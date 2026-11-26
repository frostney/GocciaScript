# Precompiled bytecode modules

## Executive Summary

- GocciaBundler compiles a source file to a `.gbc` file without running it; GocciaRunner runs a `.gbc` entry and, with `--prefer-bytecode`, takes a `.gbc` companion for an import.
- A file records its compile profile and goal; a run with other settings refuses it before any of its code runs, and a present but invalid companion is an error, never a silent fallback to source.
- Every file is read as untrusted input: bounded, verified instruction by instruction, and checked at run time where the code cannot show what a register holds.
- Bytecode modules link through the same module records as source modules: imports, cycles, live bindings and top-level `await` behave alike.

## Command line

```bash
./build/GocciaBundler library.mjs
./build/GocciaRunner --prefer-bytecode app.mjs
./build/GocciaRunner library.gbc
```

GocciaBundler writes `library.gbc` beside the source. Each input file is compiled on a thread of its own by a fresh engine (`TGocciaEngine.CompileArtifact`) under that file's settings; `--jobs` bounds how many run at once. Every function is compiled with its code, not lazily, and nothing runs. A `.gbc` input is refused (`Cannot compile bytecode file`). The bundler grants no capabilities: a config's `permissions` block is validated and any grant it requests reported as ignored. `--source-map` writes the JSX transform's map beside the bytecode file.

GocciaRunner runs a `.gbc` entry through `TGocciaEngine.RunArtifact`. The entry takes the goal recorded in the file; an explicit `--source-type` that names the other goal is rejected. A Module entry links its imports as a source module would. A Script entry keeps GocciaScript's acceptance of import declarations: the file holds their text, which runs as a module of its own before the Script ([G2 README § Script entries with imports](engine/README.md#script-entries-with-imports)).

`--prefer-bytecode`, or the `prefer-bytecode` config key, makes an import of `library.mjs` select `library.gbc` when that file exists, even if the source has been removed. Extensionless and directory-index imports find companions the same way. Without the option, imports select source; an explicit `.gbc` import loads bytecode either way. A source-phase import (`import source x`) never takes a companion, since it needs the source text.

A file that compiles as a Script by default (no static `import` or `export`, not `.mjs`) can be compiled as a Module with `--source-type=module`. Only Module files can be imported: importing a Script's `.gbc` fails with `Imported bytecode requires a linked module artifact`.

## Profiles and addresses

A `.gbc` file records the compile profile of the engine that wrote it (`CurrentArtifactProfile`): the compatibility flags, the preprocessors, strict-types, unsupported-feature warning recovery and the goal, in `TGocciaCompileProfile`'s fields (`Goccia.Bytecode.Profile`), plus whether the engine was unrestricted. `TGocciaEngine.CheckArtifact` compares them with the running engine's settings, for the entry and, through `SetArtifactCheck` in `Goccia.Modules`, for every imported file. A mismatch fails with `Bytecode compile profile does not match the engine` before anything in the file runs. Pass the same compatibility and strict-types options to the bundler and the runner.

A loaded file becomes the module record of its logical address (`ArtifactModulePath`): the `.gbc` file's directory and name with the extension of the source it was compiled from (`.js` when that had none). An import of `library.mjs` and an import of `library.gbc` therefore share one record, and a dependency that imports the original entry name reaches the same module as the `.gbc` entry. Relative imports and `import.meta` resolve against that address, not against the path the compiler saw.

Companions are held to the same rules as their sources. The resolver probes the source path through the read policy before it takes the companion (`PreferBytecodeCompanions` on the module resolver), the read capability judges the `.gbc` file it resolved to, and a companion of a CommonJS file is refused as the file itself would be. In sandbox mode imports take companions from the virtual filesystem too, but the entry must be a source file (`sandbox mode runs source files, not bytecode`).

A present companion that fails to load (a damaged file, another engine's or build's file, another profile) fails the import with `Cannot load bytecode module '<path>': <reason>`. It never falls back to the source.

## Validation

A bytecode file is untrusted input. Before reading, a file over 64 MiB (`GOCCIA_GBC_MAX_FILE_BYTES`) is refused. `LoadArtifact` then:

- checks the `GBC\0` magic, format version 200, the runtime tag `goccia-engine` and the instruction-set fingerprint, so a file from the old engine or from a build with other opcodes is refused with a message telling you to recompile
- bounds every length and count against the bytes left, nesting to 256 functions, and rejects unknown flags, constant kinds, opcodes and trailing bytes
- rebuilds constants from explicit kinds, so a file cannot forge a pointer or a hole
- verifies every template (`Goccia.ArtifactVerify.inc`): operands, jump targets, a terminator at the end, consistent exception handler depth, the prologues, and direct-eval descriptions

Instructions whose safety depends on a register holding a particular kind of value are checked at run time in code read from a file. [Bytecode VM § Binary Format](bytecode-vm.md#binary-format) describes the layout and the checks.

Files are versioned with the engine: rebuild them after upgrading GocciaScript.

## Embedding

`TGocciaEngine` (`Goccia.Embed`) exposes the same operations the applications use:

| API | Behaviour |
|-----|-----------|
| `CompileArtifact(FileName, Source, Bytes, R)` | Compiles `Source` as `Run` would (the engine's `SourceKind`, `Profile` and module-source setting) and returns the file's bytes; `False` with `R`'s error fields set for source that does not compile |
| `RunArtifact(FileName, Bytes)` | Runs the bytes as the entry, with the profile and goal checks above, and returns a `TGocciaRunResult` like `Run` |

Imports of `.gbc` files go through the module loader (`Goccia.Modules`): an explicit `.gbc` specifier, or the companion a resolver with `PreferBytecodeCompanions` selects. The loader reads files through the thread's file reader, which a host can replace with `SetFileReadHook`, as sandbox mode does for its virtual filesystem. There is no API for registering bytecode buffers under import addresses.

Lower down, `Goccia.Artifact` provides `SaveArtifact` (a compiled Script or Module to bytes; it raises `EGocciaArtifact` for a template the format cannot hold, such as a lazy function not compiled yet) and `LoadArtifact` (read and verify; the templates join the calling thread's engine and live as long as it). One engine runs per thread, so load and run bytecode on the engine's own thread.

Not supported on G2 yet: `.gbc` test files in GocciaTestRunner and `.gbc` benchmark files in GocciaBenchmarkRunner.

## Output layout and collision checks

The `--output` root directory must already exist. Directory inputs preserve their
relative tree below that root: `src/nested/index.js` becomes
`dist/nested/index.gbc` for `src/ --output=dist/`. The bundler creates missing
subdirectories there and puts generated source maps beside their bytecode files.
Each positional directory supplies its own relative root; explicit file inputs use
their basenames. Without `--output`, companions go beside their sources. With
`--multifile` on stdin, each section is written to the output directory under its
section name, with characters that are not valid in file names replaced by `_`.

Before compiling or writing, the bundler expands every positional input and
multifile section and checks all bytecode and requested map destinations. Two
outputs resolving to the same destination fail the invocation with both source
names and the destination; existing files remain untouched. This includes
same-stem inputs such as `index.js` and `index.ts`, overlapping input roots,
and an explicit map path shared by multiple inputs or by the bytecode output.
An output file also cannot occupy a directory needed by another output.
Destinations compare case-insensitively on macOS and Windows. Rename the
conflicting input or compile it separately to a distinct output.

Configured output directories keep the [config output confinement](build-system.md#configuration-file-gocciajson)
checks for every descendant. Creating nested parents does not permit following
a symbolic link outside the configured tree.

## Verification

`scripts/test-cli-aot.ts` compares source and bytecode execution through the
bundler and runner: module graphs with cycles, live bindings, deferred imports
and top-level `await`, entries in a cycle with their own imports,
extensionless and directory-index companions after the sources are removed,
package entries and CommonJS policy, read denials, sandbox companions, and
refusals of invalid, oversized and mismatched-profile files. The CLI
application suite (`scripts/test-cli-apps.ts`) runs it in PR and platform CI.

`GOCCIA_ARTIFACT_ROUNDTRIP=1` sends every compiled Script and module through a
bytecode file before it runs, and `scripts/engine/artifact-roundtrip-tests.py` and
`scripts/engine/artifact-tamper-tests.py` run the test suite that way and against
tampered files ([G2 README § Bytecode files](engine/README.md#bytecode-files)).
