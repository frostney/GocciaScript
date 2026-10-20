# Precompiled bytecode modules

## Executive Summary

- Compile module libraries once and load verified artifacts from memory.
- Register buffers under import addresses; absent disk companions can fall back to source.
- Keep compilation and execution profiles identical; mismatches fail before execution.
- Bytecode imports retain the existing module linker and its live bindings and cycles.

## API and ownership

Compile libraries once with the same language options used by the consuming
engine. `GocciaBundler library.mjs` produces an importable `library.gbc`.
In-process hosts can parse with `TGocciaSourcePipeline.Parse` and call
`Engine.CompileImportableModule(Parsed.ProgramNode)`. The engine retains the
compiled module; serialize it before freeing the engine.
Raw `TGocciaCompiler` callers must explicitly assign `Module.CompileProfile`
using `BuildBytecodeCompileProfile` from `Goccia.Bytecode.Profile.Options`
with the actual parse options and settled goal. The compiler cannot infer
parser options from an AST; serialization refuses an unstamped module.

`Goccia.Bytecode.Binary` provides:

| API | Ownership and validation |
|-----|--------------------------|
| `SaveModuleToBuffer(Module): TBytes` | Returns an owned serialized artifact |
| `LoadModuleFromBuffer(Bytes): TGocciaBytecodeModule` | Verifies the artifact; caller frees the returned module |
| `LoadModuleFromBuffer(Data, Size): TGocciaBytecodeModule` | Borrows a pointer for the call only; rejects invalid sizes and retains no pointer into the input |
| `LoadModuleFromFile(Path)` | Reads a bounded buffer and uses the same verification path |

To supply imported libraries from a cache, register their serialized buffers
under the addresses the imports resolve to:

```pascal
Engine.RegisterBytecodeModule('virtual:library', LibraryBytes);
// Source can now import from "virtual:library".
// The engine owns a copy; LibraryBytes may be changed or released.
Result := Engine.Execute;
```

`RegisterBytecodeModule(Address, Bytes, BaseAddress)` also accepts a base
address for relative registrations. It uses the existing virtual module
registry's normalization, alias and replacement rules. The registered address
governs relative imports and `import.meta`, so an artifact can be relocated
without trusting its original compiler filename. A custom content provider with
its own path namespace overrides `NormalizeEntryAddress`; the sandbox provider
normalizes buffered entries without requiring their source files to exist.

`Engine.RunBytecodeBuffer(Bytes, Address)` runs a compiled entry with normal
module linking and runtime-idle handling. It returns the entry's completion
value. Registered modules and ordinary source modules can coexist in the same
graph. No lexing, parsing or compilation of an artifact's body occurs on load.
Deferred imports, cycles, live bindings and top-level await use the same
linker as source modules. A source-phase import still needs the target's source
text.

For file-based caches, set `Engine.ModuleLoader.PreferBytecode := True`.
An import of `library.mjs` then selects `library.gbc` when present and falls
back to source when absent. An explicit `.gbc` import loads bytecode directly.
Extensionless and directory-index imports also find companions after source
removal. The stored source extension supplies the logical runtime address;
imports selecting the same companion share one module record.
A present artifact that is invalid or has the wrong profile raises an error;
it never silently falls back to source. Content-provider integrity checks and
module read capabilities still apply.

The artifact records the effective compatibility switches, preprocessors,
strict-types setting, unsupported-feature warning recovery and script/module
goal. Execution refuses a profile
mismatch before the module body runs. With `stDetect`, each entry uses the
recorded goal without changing `Engine.SourceType`; an explicit conflicting
goal is an error. Use `Engine.ValidateBytecodeModule(Module)` before retaining a low-level loaded
module or running host setup that depends on its acceptance. It returns the
settled goal without changing engine settings.

The older `RunModule` and `RunModuleInScope` APIs also check profiles for
loaded artifacts. They accept standalone bytecode only. Use
`RunModuleForSourceType(Module, Address)` or `RunBytecodeBuffer` for artifacts
with module-linking metadata so imports and declarations are initialized.

Artifacts are versioned and must be regenerated after incompatible format
changes. See [Bytecode VM — Binary Format](bytecode-vm.md#binary-format) and
[ADR 0153](adr/0153-precompiled-modules-use-the-module-linker.md).

## Command line

```bash
./build/GocciaBundler library.mjs
./build/GocciaRunner --prefer-bytecode app.mjs
```

The bundler emits `library.gbc` beside the source. `--prefer-bytecode`
selects it for an import of `library.mjs`, even if the source has been
removed. The option also accepts the `prefer-bytecode` config key. Without
the option, ordinary imports continue selecting source; an explicit `.gbc`
import selects bytecode directly.

A compiled entry uses its recorded source extension in the runtime directory,
so a dependency importing the original entry name reaches the same module.
Buffer APIs instead use the explicit address supplied by the embedder.

Pass the same compatibility and strict-types options when compiling and
running. A library with no static import/export declaration can be compiled
with `--source-type=module`. A `.gbc` entry adopts the recorded goal by
default; an explicit conflicting `--source-type` is rejected.

## Verification and measurement

`bun scripts/test-cli-aot.ts` compares source and artifact execution for
module graphs, smol-toml and a generated large library. It removes compiled
source files before running bytecode. The CLI application suite invokes it
in both PR and platform CI.

The [native loading benchmark](../benchmarks/native/README.md) measures
parse/compile against buffered loading, verification and declaration-plan
materialization, with serialization and execution excluded from both paths.
