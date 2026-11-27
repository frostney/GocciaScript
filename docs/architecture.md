# Architecture

*How source flows through the engine: the shared front end, the compiler and VM, the host layer the CLI tools run on, and the main Pascal layers.*

## Executive Summary

- **One engine** — GocciaScript runs on one engine (`source/engine`): its own compiler, register bytecode, interpreter, baseline JIT and optimizing tier (Linux x86-64), object model, generational GC and built-ins. Goccia's lexer, parser and AST (`source/units`) are its front end
- **Embedding API** — `TGocciaEngine` (`Goccia.Embed`) is what hosts build on: it makes the thread's engine, takes the run's settings, and `Run` compiles and runs one entry as a Script or a module, then its event loop, and reports how the run ended in a `TGocciaRunResult`
- **One engine per thread** — every piece of engine state (heap, atoms, shapes, stacks, built-ins, job queue) is a threadvar, so a thread has at most one engine and values never cross threads; the CLI tools run each file on a thread of its own
- **Source pipeline** — `TGocciaSourcePipeline` (preprocessors, compatibility flags, warnings, source type) parses entries and modules under a restricted language profile; the whole-language profile parses with the lexer and parser directly and compiles functions lazily
- **Host as preludes** — the CLI host's globals and `goccia:*` modules are JavaScript preludes (`source/engine/prelude`) over natives in `Goccia.HostAPI`; the core built-ins are native Pascal plus engine preludes compiled at build time into a snapshot
- **Execution state** — no evaluation-context record or scope objects: a run's state is the thread's VM state, a function's bindings are registers and upvalue cells, and Script and module bindings live in cell tables. See [Bytecode VM](bytecode-vm.md)

## Overview

GocciaScript executes source one way: the front end parses it into Goccia's AST, `Goccia.Compiler` compiles the AST to register bytecode, and `Goccia.VM` runs it. The engine replaced the old engine, and every application in `source/app` (GocciaRunner, GocciaTestRunner, GocciaBenchmarkRunner, GocciaBundler, GocciaREPL, GocciaTest262Runner, GocciaScriptLoaderBare, GocciaFuzzHarness) runs on it ([ADR 0188](adr/0188-the-delphi-projects-and-the-wasm-test-runner-leave-with-the-old-engine.md)). It builds with FreePascal 3.3.1 only, because it relies on native-TLS threadvars; [Engine](engine/README.md) and [Engine design](engine/design.md) describe the engine in more depth.

The source pipeline public API is `TGocciaSourcePipeline` in `Goccia.SourcePipeline`. `Parse` accepts source text plus `TGocciaSourcePipelineOptions` (preprocessors, compatibility flag set, unsupported-feature warning policy, and source type) and returns an owning `TGocciaSourcePipelineResult` containing the AST, generated-source lines, source map, timings, and warnings as data. The engine parses through it whenever the language profile is restricted, as the CLI tools' profile is (`HostCompileEntry` and `ParseAndCompileModule` in `Goccia.Eval`, which turn the profile into pipeline options): the JSX preprocessor runs, the profile's compatibility flags become the pipeline's, and its warnings reach the host (`TGocciaEngine.OnWarning`, `OnModuleWarning`). Each AST node reports the nodes beneath it, in source order, through `TGocciaASTNode.ForEachChild`; `Goccia.Compiler`'s and `Goccia.Eval`'s AST scans and the `goccia:ast` module's report (`Goccia.AST.Report`) walk that instead of keeping their own `case` over the node classes ([ADR 0162](adr/0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)).

The parser owns ECMAScript lexical-goal choices. `TGocciaLexer.ScanNextToken` scans one token at a time with an explicit `TGocciaLexicalGoal` such as `InputElementRegExp` or `InputElementDiv`; parser lookahead requests the goal required by the syntactic context before ambiguous input is classified. This follows the ES2026 lexical grammar split (ES2026 §12) between `InputElementDiv`, `InputElementRegExp`, `InputElementRegExpOrTemplateTail`, `InputElementTemplateTail`, and `InputElementHashbangOrRegExp`. Template literals use the same model: the lexer emits `TemplateHead`, `TemplateMiddle`, and `TemplateTail` span tokens, while `${...}` expressions are parsed in the main parser stream before the parser requests the next template-tail span.

Under the unrestricted profile, the whole language as the conformance host and the preludes run it, the engine compiles lazily (`Goccia.Lazy`; `GOCCIA_LAZY=0` turns it off). `Goccia.PreParse` reads the source with the engine's own scanner (`Goccia.Scan`) and checks the whole syntactic grammar and every early error without building a tree, recording each function's source range and free names. Goccia's parser then builds the AST with the eligible function bodies skipped, and `Goccia.Compiler` makes each skipped function a stub template whose first call parses and compiles its own source range. Lazy compilation is off under a restricted profile, because the pre-parser does not apply a restricted profile's early errors to the bodies it skips.

## Pipeline

```text
Source -> Preprocessors (optional, e.g. JSX) -> Lexer -> Parser -> Goccia.Compiler -> bytecode
       -> Goccia.VM: Execute / FastLoop -> baseline JIT -> optimizing tier (Linux x86-64) -> TValue
```

## Main Layers

| Layer | Units | Responsibility |
|-------|-------|----------------|
| Embedding API | `Goccia.Embed`, `Goccia.Engine` | `TGocciaEngine`: the thread's engine, run settings (language profile, source kind, limits, capabilities, host config), `Run`, `RunArtifact`, `CompileArtifact`, `RunTests`, `RunBenchmarks`; `EngineInit` / `EngineDone` underneath |
| CLI host | `Goccia.HostAPI`, the host preludes in `source/engine/prelude`, `Goccia.Fetch`, `Goccia.FFI`, `Goccia.Sandbox`, `Goccia.HostSemver`, `Goccia.Profile`, `Goccia.HeapSnapshot` | The globals and modules the CLI tools give a script beyond the language (console, timers, URL, fetch, `goccia:*` data modules, testing and benchmark APIs), the event loop, module resolution hooks, globals injection and virtual modules |
| CLI applications | `source/app` (`Goccia.CLI.Application`, `Goccia.CLI.FileEngine`, `Goccia.CLI.SandboxHost`, the `.dpr` programs) | Options, configuration and trust, file discovery, worker threads, reports |
| Front end (shared) | `Goccia.SourcePipeline`, `Goccia.JSX.Transformer`, `Goccia.Lexer`, `Goccia.Parser`, `Goccia.AST.*` | Source text to AST, parser policy, preprocessor dispatch, source maps, warnings |
| Lazy front end | `Goccia.Scan`, `Goccia.PreParse`, `Goccia.Lazy`, `Goccia.Eval` | Pre-parsing and early errors for lazy compilation; `Goccia.Eval` is the entry to parsing and compiling at run time (entries, modules, `eval`, the `Function` constructor) |
| Compiler | `Goccia.Compiler` | Goccia AST to register bytecode, one walk per function |
| Bytecode | `Goccia.Instructions`, `Goccia.Artifact`, `Goccia.Snapshot` | Instruction format and function templates; `.gbc` files (untrusted input, verified); the build's prelude snapshot |
| VM | `Goccia.VM` (with `Goccia.Jit.inc` and `Goccia.Opt.inc`) | Frames on one value stack, upvalues, exception handlers, inline caches, realms, execution limits; `Execute`, `FastLoop`, the baseline JIT and the optimizing tier |
| Values and heap | `Goccia.Core` | NaN-boxed `TValue`, cells, strings and atoms, shapes, objects, the generational GC and the memory limit |
| Object model | `Goccia.ObjModel`, `Goccia.Weak` | Property descriptors and the exotic objects' internal methods; WeakRef and FinalizationRegistry |
| Built-ins | `Goccia.Builtins` to `Goccia.Builtins5`, `Goccia.ArrayProto`, `Goccia.Typed`, `Goccia.TypedArrays`, `Goccia.Promise`, `Goccia.RegExpV`, `Goccia.Regex`, `Goccia.JSON`, `Goccia.Date`, `Goccia.Intl`, `Goccia.Temporal`, `Goccia.Atomics`, `Goccia.Shared`, `Goccia.ShadowRealm`, `Goccia.Disposal`, `Goccia.IterHelpers`, `Goccia.BigIntV` | The ECMA-262 and ECMA-402 built-ins, native or as engine preludes |
| Modules | `Goccia.Modules` | Module records loaded from files or the host, linking, evaluation, namespaces, `import()` and `import.meta` |
| Shared services | `Goccia.Capabilities`, `Goccia.CapabilityAudit`, `Goccia.ModuleResolver`, `Goccia.Modules.Resolver`, `Goccia.RegExp.Compiler`, `Goccia.YAML`, `Goccia.TOML`, `Goccia.DataTree`, `Goccia.Temporal.TimeZone`, `Goccia.Coverage`, `Goccia.Profiler` | Engine-independent units the host and built-ins call |

For **bytecode execution**, see [Bytecode VM](bytecode-vm.md). For **canonical terminology**, see [GocciaScript Context](../CONTEXT.md). For **recurring implementation patterns** and Define vs Assign implementation details, see [Core patterns](core-patterns.md).

The engine also owns what source may reach outside the process: the `TGocciaCapabilities` set a host gives `TGocciaEngine.Capabilities` before the host is prepared (none when it gives none). `fetch` judges every destination by it (`Goccia.Fetch`, whose worker threads carry the set with each request), the read policy judges module reads by it (`Goccia.HostAPI`), and the `FFI` global is installed only when it grants `ffi`; see [Permissions](permissions.md).

Source type belongs to `TGocciaEngine.SourceKind`: `gskScript`, `gskModule`, or `gskDetect`, the default, under which a `.mjs` file is a module and otherwise the source pipeline applies the [entry source type policy](language.md#modules) ([ADR 0140](adr/0140-an-entry-with-module-syntax-runs-as-a-module.md)). An explicit Script entry may still contain import declarations: the engine runs them as a module of their own and binds each imported name as a const global lexical binding before the Script runs ([Engine](engine/README.md)).

## Design Direction

- Bytecode and its VM are specific to the engine, not a generic VM layer: 64-bit instructions (op 8 bits, A 16, B 16, C 24) over a register frame, with inline caches keyed by shape.
- `TValue` is a NaN-boxed `QWord`: doubles stand for themselves, and tags mark an Int32, a special value (`undefined`, `null`, `false`, `true`, the array hole) or a heap cell pointer. Numbers never allocate, in registers or anywhere else.
- Arrays, objects, functions and promises are `ckObject` cells whose object kind (`TObj.OKind`) selects their behaviour; property storage is a shape plus slots.
- Sparse arrays use the hole value `VHole`, a special value no script can produce.
- Hot paths run in tiers: `Execute` handles every instruction, `FastLoop` the hot ones with its loop variables in machine registers, and on Linux x86-64 the baseline JIT and the optimizing tier compile hot code. Every tier stops and hands an instruction back to a slower one when its fast path does not apply.
- Precompiled bytecode uses the engine's `.gbc` format (`Goccia.Artifact`); see [Bytecode VM](bytecode-vm.md).

## CLI Library

The CLI tools share a two-level application class hierarchy and a declarative option parsing system.

**Application classes:**

- `TGocciaApplication` (`Goccia.Application.pas`) — embeddable base for any GocciaScript host. Provides `Run`/`RunApplication`, which turn a `TCLIUsageError` into exit code 2 (`EXIT_CODE_USAGE`) and any other exception into exit code 1 after unified error handling (`HandleError` virtual). It does not depend on the CLI application layer and has no engine code: each run's `TGocciaEngine` sets up its own heap.
- `TGocciaCLIApplication` (`Goccia.CLI.Application.pas`) — extends `TGocciaApplication` with CLI concerns: argument parsing, help generation, option registration, configuration trust, the `--log` and `--audit-log` files, and the coverage/profiler singleton lifecycle. Tools override `Configure` (register options) and `ExecuteWithPaths` (business logic).

**Option class hierarchy**:

- `CLI.Options.pas` provides generic primitives: `TOptionBase` → `TFlagOption`, `TStringOption` (→ `TOptionalStringOption`), `TIntegerOption`, `TInt64Option` (→ `TByteSizeOption`, `TDurationOption`, `TCountOption` for unit-aware limits), `TScopeListOption` (`--allow-<cap>[=scope,...]` grant lists), `TRemovedOption` (a removed flag that stays parseable so its use fails with a message naming the replacement), `TRepeatableOption`, `TEnumOption<T>`
- The parser calls `Option.Apply(Value)` via virtual dispatch — no pointer arithmetic
- `Goccia.CLI.Options.pas` owns Goccia-specific groups (`TGocciaEngineOptions`, which owns `TGocciaCapabilityOptions` for the `--allow-*`/`--deny-*` flags; `TGocciaCoverageOptions`; `TGocciaProfilerOptions`; `TGocciaSandboxOptions`) and the compatibility flag registry used by each CLI host

**CLI lifecycle** (`TGocciaCLIApplication.Execute`):

1. `Configure` — register option groups and tool-specific flags
2. `ParseCommandLine` — parse `ParamStr` via virtual `Apply` dispatch; the trust modes run here and end the command
3. `ValidateCommandLine` — the tool's own checks, before the shared capability and limit checks
4. `Validate` — post-parse semantic checks (e.g., conflicting flags)
5. `InitializeSingletons` — coverage tracker, profiler
6. `ExecuteWithPaths` — tool business logic
7. `AfterExecute` — reporting hooks
8. `ShutdownSingletons` — cleanup in reverse order

`Goccia.CLI.SourceMaps.pas` owns the shared CLI source-map file output policy used by the Runner and Bundler. What GocciaRunner and GocciaTestRunner share for one file's engine (console capture, file settings, the import capability over the file's module resolution) is `Goccia.CLI.FileEngine`.

**Tool mapping:**

| Tool | Base Class | Overrides |
|------|-----------|-----------|
| GocciaREPL | `TGocciaCLIApplication` | `Configure`, `ExecuteWithPaths` |
| GocciaRunner | `TGocciaCLIApplication` | `Configure`, `ValidateCommandLine`, `Validate`, `ExecuteWithPaths`, `HandleError`, `AfterExecute` |
| GocciaTestRunner | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths` |
| GocciaBenchmarkRunner | `TGocciaCLIApplication` | `Configure`, `Validate`, `ExecuteWithPaths`, `AfterExecute` |
| GocciaBundler | `TGocciaCLIApplication` | `Configure`, `ExecuteWithPaths`, `HandleError` |

GocciaTest262Runner, GocciaScriptLoaderBare and GocciaFuzzHarness are plain programs with their own command lines. Each tool runs a file's engine on a worker thread (GocciaRunner's `TGocciaFileThread`, for example), at most `--jobs` at once, because an engine belongs to the thread that made it. The baseline JIT compiles on one thread at a time (its emitter's buffers are shared), so a host whose engine runs on another thread hands compiling to that thread with `JitTakeThread`, as GocciaRunner's file thread does.

`GocciaRunner` has a host mode and a [sandbox mode](permissions.md). In sandbox mode `Goccia.CLI.SandboxHost` copies the host inputs into a `TSandboxVirtualFileSystem` and captures that as the seed baseline, then runs the program on a thread of its own with a fresh engine whose thread is given the sandbox context (`Goccia.Sandbox`): its modules and data files are read from the virtual file system, and `prelude/sandbox.js` installs the `"fs"`, `"goccia"` and `"goccia:search"` modules.

A nested run (`runScript`, or the shell's `goccia`) gets a `TGocciaSandboxContext` of its own and runs on a new thread with a new engine, while its caller's thread waits in the native: an engine's state is its thread's, so the child cannot share the caller's thread. The child inherits its caller's capability set, and its completion value comes back encoded as text (primitives, arrays and own data properties, cycles cut) that the caller decodes in its own realm.

## Execution Architecture

`TGocciaEngine.Create` calls `EngineInit`, which builds the thread's engine: it masks the floating-point exceptions on the thread (so NaN and infinity arithmetic follows IEEE 754 instead of trapping), sets up the heap (`HeapInit`) and the VM (`VMInit`), and runs the intrinsics' Init procedures (`InitIntrinsics`), which make the first realm's built-ins and run the engine preludes. `TGocciaEngine.Destroy` releases the host's per-thread state (resolver, fetch requests, FFI) and calls `EngineDone`, which frees the heap, every function template and the preludes' lazy sources; values and closures from the engine are invalid afterwards.

The host's own globals and modules are installed once, before the first run (`TGocciaEngine.PrepareHost`, which `Run` calls when the host has not): `InstallHostGlobals` in `Goccia.HostAPI` calls the host preludes with the global object, the host's natives and an object of shared internals. The engine's flags decide what the host adds: `Bare` keeps only GocciaScriptLoaderBare's surface, `ExposeTimers` the timer globals, `TestMode` the test host's additions (the `vitest` module unless the config turns it off), `Capabilities` the `FFI` global; GocciaTestRunner and GocciaBenchmarkRunner then install the testing API (`InstallTestingAPI`) or the benchmark API (`InstallBenchmarkAPI`) themselves. The conformance host (`goccia --test262`, `Goccia.Test262`) installs none of it.

`TGocciaEngine.Run(FileName, Source)` then:

1. applies the run's settings: the heap limit (`MaxMemory`), the language profile, the console sink, the read policy, the resolver and the module content checks;
2. loads the [host environment](host-environment.md) module, when one is named;
3. compiles the entry (`HostCompileEntry`) under `SourceKind`, or loads it from a `.gbc` file (`RunArtifact`);
4. arms the limits (`LimitsArm`: timeout, instruction budget, frame bound);
5. runs it: a Script through `RunScriptRes`, after its import declarations' module when it has some; a module through `RunModuleFile`;
6. runs the event loop (`RunEventLoop`, see [Synchronous Microtask Queue](#synchronous-microtask-queue));
7. fails the run on an unhandled rejection when `FailOnUnhandledRejection` is set, and on an entry module whose top-level `await` nothing can settle any more (`TypeError: await: Promise did not settle`);
8. converts the completion value when `ResultForm` asks for it, and fills the `TGocciaRunResult`: `Ok`, `Aborted` (timeout, instruction limit, memory limit or host abort), the error in parts with its location and suggestion, and the compile and execution times.

Settings that shape compilation are engine properties, read when the run starts: `Profile` (a `TLangProfile`: the compatibility flags, strict types, the `Function` constructor and ShadowRealm switches), `SourceKind` and `JSModuleSources`. `RecommendedProfile`, the constructor's default, is restricted with every compatibility switch off. See [Embedding](embedding.md) for the API.

### Evaluation Context

The engine has no evaluation-context record. What the old engine passed between engine, module loader and scopes is the engine thread's own state: the VM's frames, handler stack and value stack, the pending exception (`Pending`), the running realm (`CurRealm`), the global lexical bindings, the module records of `Goccia.Modules`, the job queue of `Goccia.Promise`, and the hooks a host sets for its thread (`SetHostResolveHook`, `SetHostModuleHook`, `SetFileReadHook`, `SetModuleContentCheck`). Because each of these is a threadvar, two engines on two threads share none of it, and a native needs no context parameter to reach its engine.

What relative imports resolve against is the importing module record's path: each record is keyed by its absolute path (a host module by its specifier, such as `goccia:csv`), and `import()` passes the referrer's source name to `Goccia.Modules` (`ImportHook`).

### Scope Chain Design

There are no runtime scope objects. `Goccia.Compiler` resolves every identifier at compile time to one of these:

- **A register** — a function's parameters, locals and temporaries live in its frame's registers on the VM's value stack (R0 is `this`).
- **An upvalue** — a variable a closure captures stays an open upvalue on the stack while its frame lives and is closed into a `ckUpval` cell when its scope ends (`opClose`), as in Lua. The closure reaches it with `opGetUpv` / `opSetUpv`.
- **A global lexical binding** — `let`, `const` and `class` at a Script's top level stay registers of the Script's frame, so its own code keeps register access, and `opMakeGLex` publishes their cells in the realm's global lexical table (`GLexCells`, `GLexNames`, `GLexConst`). Other Scripts reach them through `opGetGLex` and `opSetGLex`, finding them by name (`GLexFind`); assigning to a `const` one throws a `TypeError`.
- **A global object property** — a name no lexical binding declares, through `opGetGlobal` / `opSetGlobal` and their inline caches. A Script's top-level `var` and function declarations (with `--compat-var` and `--compat-function`) are bindings of the global object, which `opDeclGlobals` and `opDefGlobalFunc` declare before the Script's first statement (ES2026 §16.1.7 GlobalDeclarationInstantiation); a function's `var` bindings are registers ([Language § `var` Declarations](language.md#var-declarations)).
- **A module cell** — a module compiles to a generator over a table of binding cells (`opMakeModCell`); an import is the exporter's cell in the importer's table, read with `opModGet`.

Rules that come with it:

- **Temporal Dead Zone** — a lexical binding holds the hole until its declaration runs. The compiler emits the check (`opCheckHole`) only where a read or write can precede the declaration, and it throws a `ReferenceError`; `opGetGLex` and `opSetGLex` check a global lexical binding on every access.
- **Module scope isolation** — a module's top-level bindings are registers and cells of its own function, so nothing of them reaches the global object or another module except through exports.
- **`with` and direct `eval`** — `with` compiles to object-environment instructions (`opWithObj`, `opWithHas`, `opWithGet`, `opWithSet`). A direct `eval` in function code compiles its source as an arrow inside a stand-in for its caller: the call site records every binding it sees, and the eval code reaches them as upvalues ([ADR 0134](adr/0134-direct-eval-compiles-as-a-closure-of-its-call-site.md)).
- **Compatibility semantics** — the `arguments` object, non-strict `this`, assignment and `delete` compile to explicit VM state and instructions; see [Language](language.md) for the behavior.

### Modules and Data Formats

- **Module path resolution** — The engine asks the host's resolution hook first. The CLI host resolves its own modules (`goccia:*`, virtual modules, `vi.mock` mocks) by their keys and anything else through the run's `TGocciaModuleResolver` (`TGocciaEngine.Resolver`): import-map aliases (exact match for keys without `/`, prefix match for keys with `/`, longest matching key wins), `./` and `../` relative to the importing file, the extensions of `ModuleImportExtensions` in order for an extensionless path (`.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`, `.mts`, `.json`, `.json5`, `.jsonc`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, `.tsv`, `.txt`, `.md`), and bare specifiers through `node_modules` and provider packages. `TGocciaModuleResolver.LoadImportMap` resolves import-map values relative to the map file and `DiscoverProjectConfig` walks parent directories looking for `goccia.json`. CLI applications also discover a project-level `goccia.toml`, `goccia.json5`, or `goccia.json` (in that priority order) via `CLI.ConfigFile.DiscoverConfigFile`; config keys mirror CLI option names and CLI options override them. See [Module Resolution](module-resolution.md).
- **Linking and evaluation** — `Goccia.Modules` implements Source Text Module Records (ES2026 §16.2.1.6 Cyclic Module Records). Calling a module's compiled function is its environment's initialization (function declarations made, exported bindings published as cells); resuming it runs the body. Linking runs every module's start first, then fills the import slots, so modules in a cycle see each other's functions. A module with top-level `await` compiles to an async generator, and its evaluation waits for it, a simplification of the specification's asynchronous evaluation order.
- **Live import bindings** — an import is the exporter's own cell, so every read observes the binding's current value, and a read before the exporter initialized it throws a `ReferenceError` through `opModGet`.
- **Namespace imports** — `import * as ns` binds a module namespace object (`okModuleNS`, ES2026 §10.4.6 Module Namespace Exotic Objects) holding its exports' keys and cells sorted by key.
- **Module reload** — a module record read from a file remembers when the file was last modified; a request that finds the record settled and the file newer loads it again into a new record ([ADR 0139](adr/0139-a-reload-links-into-the-cached-module-record.md)).
- **Data and text modules** — the host's `shared.dataFormats` table (`prelude/modules.js`) maps an extension to a function from the file's text to its exports. A JSON file exports its top-level object's properties and `default`; `.txt` and `.md` export `content` and `default` (the text with line breaks as LF) and a frozen `metadata` object (`kind`, `path`, `fileName`, `extension`, `byteLength`). A JSON module imported with `with { type: 'json' }` goes through `Goccia.Modules` and `%JSON.parse%`'s grammar (`Goccia.JSON`), and takes the host's exports when the host sets `JSONExportsHook`.
- **Data format parsers** — JSON is `Goccia.JSON` (`JSON.parse` and `JSON.stringify`, ES2026 §25.5). JSON5 and `.jsonc` (`prelude/json5.js`), JSONL (`prelude/jsonl.js`) and CSV and TSV (`prelude/delimited.js`) are JavaScript preludes that back their `goccia:*` modules and file imports. YAML and TOML are parsed in Pascal by `Goccia.YAML` and `Goccia.TOML` into an engine-free `Goccia.DataTree` tree; the `dataParse` native hands it over as JSON text, and `prelude/datatree.js` rebuilds it, so a node a YAML alias shares is one value, cycles included. The same parsers back `--globals` files (`TGocciaEngine.InjectDataGlobals`). See [Data Formats](built-ins-data-formats.md) for each format's behavior.

### Error Handling Strategy

GocciaScript uses a layered error approach (see [Errors](errors.md) for the full error type reference and user-facing display format):

1. **Compile-time errors** — Goccia's lexer and parser raise Pascal exceptions (`TGocciaSyntaxError` and its subclasses); `Goccia.Compiler` raises its own early errors the same way. `Goccia.Eval` catches them: an entry's error goes to the host in the `TGocciaRunResult` (name, message, line, column and the parser's suggestion), while code compiled at run time (`eval`, the `Function` constructor, `ShadowRealm.prototype.evaluate`) throws a `SyntaxError` value.
2. **JavaScript-level errors** are thrown values, never Pascal exceptions. A native (`TNativeFn`) that throws sets the thread's `Pending` value and returns `False`; every helper that can run code (`CallValue`, `GetV`, `SetV`, `ToStringV`, ...) passes that convention on. The VM unwinds a throw to the innermost handler record (`THandler`: frame, PC, catch or finally) on its own handler stack; the dispatch loop has no Pascal `try`. A throw with no handler ends the run with `Pending` holding the value.
3. **`break`, `continue` and `return`** compile to jumps and returns. When one leaves a `try` that has a `finally`, the compiler ends the handler (`opEndTry`) and emits the `finally` block's code at that exit (`EmitFinallyUnwind`), closing a `for-of` loop's iterator and disposing a `using` block's resources there too.
4. **Spent limits** — a timeout, an instruction budget, a memory limit that refusals no longer relieve, or a host abort set `Aborting`: the pending error then passes every `catch` and `finally`, and no further job runs. The host reads which limit it was from `TGocciaRunResult.Aborted`.
5. **Host errors** — what goes wrong outside a script (a globals file that does not parse, a module the host failed to load) reaches the host as an error message from the `TGocciaEngine` method it called, or through `DescribeHostError`.

**Error construction** — `Goccia.VM` provides `ThrowError(Kind, Msg)`, `ThrowTypeError` and `MakeError(Proto, Msg)`; the error gets its `stack` from `AttachStack` when the profile wants stack traces. Message and suggestion texts shared with the CLIs come from `Goccia.Error.Messages` and `Goccia.Error.Suggestions`, and a host's suggestion for an error it throws is kept in a slot the script cannot see (`SetErrorHostSuggestion`).

### Synchronous Microtask Queue

GocciaScript implements ECMAScript Promises with a synchronous job queue that drains after the script, as the event loop's first step.

**The queue:** `Goccia.Promise` keeps the jobs as quads `[kind, a, b, c]` in one ring buffer per engine (the threadvar `Jobs`, a GC root), so queueing a job allocates nothing until the ring grows. A job is a promise reaction (`jkReaction`), a thenable's adoption (`jkThenable`, ES2026 §27.2.2.2 NewPromiseResolveThenableJob), or a callback (`jkCallback`: `queueMicrotask`, `import()`'s loading). `RunMicrotasks` runs them in FIFO order until the queue is empty, so a job queued while draining runs in the same drain. Before each job it clears the WeakRef kept-objects list (ES2026 §9.10 ClearKeptObjects).

**The event loop:** `RunEventLoop` (`Goccia.HostAPI`) repeats, until nothing is left:

1. the microtasks;
2. the async waits of `Atomics.waitAsync` that other threads notified or whose timeouts passed (`RunHostLoop`, `PumpAsyncWaits` in `Goccia.Atomics`);
3. one FinalizationRegistry cleanup job, then the microtasks it queued (ES2026 §9.9.4.1 HostEnqueueFinalizationRegistryCleanupJob);
4. the `fetch` requests that finished, waiting for those still in flight before any timer fires (`Goccia.Fetch`);
5. the next timer on the [virtual timer queue](adr/0113-deterministic-virtual-timer-queue.md) (`prelude/timers.js`, `shared.timers.fireNext`). Nothing there waits on wall time: the clock jumps to the next due timer when the engine has nothing else to do, or moves only when a test moves it under fake timers. Each timer is followed by the microtasks it queued, which is the macrotask-then-microtask order of an event loop without real elapsed time.

When [an embedder has outstanding work](embedding.md#asynchronous-host-work), the loop also
pumps its ready results and drains the reactions they queue. It polls ready Atomics and fetch
results without blocking on either. An in-flight fetch still prevents a virtual timer jump.
If only native work can make progress, the loop gives the embedder a bounded wait before it
checks all sources again.

When nothing is left but a FinalizationRegistry cell still waits on its target, the loop collects once, so a cleanup that came due still runs.

**Why drain after the script, not during it?** In the ECMAScript specification a job runs only when the execution context stack is empty ([ES2026 §9.5](https://tc39.es/ecma262/#sec-jobs)): all synchronous code runs to completion first, then the reactions in FIFO order. A promise resolved with a thenable takes one job to adopt it and another for the reaction.

**Nested runs:** each engine has its own thread and queue, so a nested run (a sandbox `runScript`) drains only its own jobs; its caller's are still queued when it returns. A ShadowRealm is a second realm of the same engine and shares its queue.

**Unhandled rejections:** rejections are tracked as ES2026 §27.2.1.9 HostPromiseRejectionTracker allows (`TakeUnhandledRejection`); with `TGocciaEngine.FailOnUnhandledRejection` a rejection still without a handler when the loop ends fails the run with its reason, and the test runner attributes each to the test or hook that left it ([Errors](errors.md)).

**`queueMicrotask`:** enqueues a callback job in the same queue as Promise reactions ([HTML](https://html.spec.whatwg.org/multipage/timers-and-user-prompts.html#microtask-queuing)). A callback that throws ends the run as an uncaught exception, except in a test file, where it fails the unit that queued it.

**Spent limits:** `RunMicrotasks` stops as soon as a limit is spent (`Aborting`), even when the job that met it caught the error, so no further job runs.

**Integration points:**

| Context | When jobs drain |
|---------|-----------------|
| `TGocciaEngine.Run` | After a Script's imports evaluated, and after the entry Script or module (`RunEventLoop`) |
| `TGocciaEngine.RunTests` | After the test file's suites ran, including the work the tests left |
| `TGocciaEngine.RunBenchmarks` | After the benchmarks were measured |
| `EngineRunScript` (`Goccia.Engine`) | After the script: microtasks and `Atomics.waitAsync` only, no host timers |

### Shared Semantic Helpers

ECMAScript abstract operations that natives and the VM both need are routines of `Goccia.VM` that take and return `TValue`s: `ToPrimitiveV` and `ToPrimitiveHint` (ES2026 §7.1.1 ToPrimitive), `OrdinaryToPrimitive`, `ToNumberV`, `ToStringV`, `ToPropKey`, `ToBigIntV`, `StrictEquals`, `LooseEquals`, `InstanceOf`, `GetIteratorRec` and `IteratorStepRec`. Conversions that never call JavaScript live in `Goccia.Core` (`PrimitiveToNumber`, `NumberToStr`, `ToBool`, `TypeOfStr`).

**IEEE-754 correctness:** Number arithmetic works on doubles with the floating-point exceptions masked on the engine's thread. `MkArith` boxes the result of `+`, `-`, `*` and `/` and canonicalizes a NaN the hardware produced, so every boxed NaN is the canonical one. `NumberRemainder` is ES2026 §6.1.6.1.6 Number::remainder (the truncating remainder with the dividend's sign), and `NumberExponentiate` is ES2026 §6.1.6.1.3 Number::exponentiate over the C library's `pow`, taking a positive finite base to the power 0.5 as its square root so that result is correctly rounded on every target. `DTrunc`, `DFloor`, `DCeil` and `DRound` work over a double's whole range and keep `-0` where the specification keeps it.

## Realm Isolation

A realm (ES2026 §9.3) is the running realm's intrinsics and global environment, held in threadvars while it runs. Each intrinsic's Init procedure registers the threadvars that hold its realm's state (`RealmVals`, `RealmPtrs`, `RealmInts`). `CreateRealm` (ES2026 §9.3.1 InitializeHostDefinedRealm, for a ShadowRealm or test262's `$262.createRealm`) runs the Init procedures again for the new realm, and `SwitchRealm` saves the running realm's registered values, its global lexical table and its `var` names, and loads another's. A function object carries its realm's index, so a call to another realm's function switches to that realm and the caller's comes back when the callee returns. See [Core patterns § Realm Ownership & Slot Registration](core-patterns.md#realm-ownership--slot-registration) for the registration recipe.

```text
engine thread (one TGocciaEngine)
  ├── heap, atoms, shapes, VM stacks, job queue      (threadvars: one per engine)
  ├── realm 0 (running)  -> its intrinsics and global environment in the registered threadvars
  └── Realms[1..]        -> each other realm's saved values, loaded by SwitchRealm
```

Engines do not share realms or intrinsics: a new engine on a thread starts from `EngineInit`, which builds its first realm from scratch, and `EngineDone` frees everything the previous one made. Mutations of `Array.prototype` therefore never reach another engine. See [ADR 0032](adr/0032-per-engine-realm-isolation.md) for the original rationale and [Embedding](embedding.md) for embedder-facing implications.

## Duplication Boundaries (beneficial vs harmful)

The front end, the compiler and the VM are separate layers with separate representations of the same specification: the parser works on source, the compiler on the AST, the VM on instructions and registers. Sharing the **same** `TValue` model and the same property access routines between the VM, the built-ins and the host is the consolidation point.

- **Beneficial separation** — Different layers solving different problems: Goccia's parser (which builds the AST) and `Goccia.PreParse` (which checks the grammar and early errors without one, for lazy compilation); `Goccia.Compiler` and `Goccia.VM`; engine-independent format parsers (`Goccia.YAML`, `Goccia.TOML`) and the thin host modules over them; a built-in's spec steps in a prelude and its native fast path, which runs only while the objects involved are unmodified. The execution tiers are the same kind of separation: `Execute`, `FastLoop`, the baseline JIT and the optimizing tier each implement fast paths of the same instruction, and each hands back to `Execute` whatever its fast path does not cover.

- **Harmful duplication** — The **same rule** maintained twice without a seam, so that it can drift: a check that one tier makes and another omits, or an early error the pre-parser and the parser judge differently. Keep such a rule in one routine the tiers share, or make the slower tier the only one that decides it. GocciaFuzzHarness's `--grammar` mode compares a run without compiled code against a run with the JIT for exactly this kind of drift, and a field definition's receiver check is made in every tier for the same reason ([Engine § Bytecode files](engine/README.md#bytecode-files)).

When in doubt: preserve pipeline separation; consolidate **policy and mechanical helpers**.

## Related Documents

- [Bytecode VM](bytecode-vm.md) — Compiler, instructions, register VM
- [Core patterns](core-patterns.md) — Recurring implementation patterns
- [Engine](engine/README.md) and [Engine design](engine/design.md) — The engine's status, build, hosts and design
- [GocciaScript Context](../CONTEXT.md) — Canonical project terminology
- [Build System](build-system.md)
- [Architecture Decision Records](adr/)
- [Contributing](../CONTRIBUTING.md) — Single contribution standard (workflow, mandatory rules, testing, Pascal style, quick reference)
