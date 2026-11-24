# Embedding the Engine

*For Pascal developers who want to embed the GocciaScript engine in their own applications.*

## Executive Summary

- **Quick start** — `TG2Engine.Create` (unit `G2.Embed`) builds an engine on the calling thread; `Run(FileName, Source)` compiles and runs one entry as a Script or a Module, then its jobs and timers, and returns a `TG2RunResult` instead of raising
- **Host surface** — The first run (or `PrepareHost`) installs the CLI hosts' globals and `goccia:` modules; `Bare` leaves only the core language, and settings such as `ExposeTimers`, `ConfigJson`, `Profile` and `Capabilities` choose what else a script sees
- **Sandboxing** — An optional immutable [capability set](permissions.md) judges module reads, `fetch` and `FFI`; an `AuditSink` receives every decision; `TimeoutMs`, `MaxInstructions`, `MaxMemory` and `MaxFrames` bound a run; `TG2SandboxHost` runs scripts over an in-memory filesystem
- **Globals and modules** — Inject globals from JSON, data files or Pascal natives; resolve imports through a `TGocciaModuleResolver` with aliases and import maps; define virtual modules; receive parse warnings through `OnWarning` and `OnModuleWarning`
- **One engine per thread** — Every piece of engine state is a threadvar (`G2.Engine`), so engines on different threads run side by side and share nothing

Embedding is GocciaScript's primary goal ([Vision](../VISION.md#primary-an-engine-for-embedding)). The engine is G2 ([G2 overview](g2/README.md), [design](g2/design.md)): `G2.Compiler` compiles the front end's syntax tree (`Goccia.Parser`, `Goccia.SourcePipeline`) to register bytecode, `G2.VM` runs it in an interpreter, and on Linux x86-64 a baseline JIT and an optimizing tier compile hot code. `TG2Engine` in `G2.Embed` is the API the applications in `source/app` build on, and the one this page describes.

## Building an Embedding

G2 needs FPC 3.3.1 from fpc-toolchain; `build.pas` refuses an older compiler. A program that embeds the engine compiles with the paths `build.pas` passes for its own G2 programs (`G2Args`):

- units from `source/g2`, `source/units`, `source/shared` and `source/generated` (and `source/app` for `Goccia.CLI.*` helpers such as `TG2SandboxHost`);
- includes from the same directories, plus `build/g2/gen`, where `build.pas` writes each JS prelude of `source/g2/prelude` as a Pascal constant when it builds any G2 program, and a directory holding `G2.PreludeSnapshot.inc`;
- the compiled resources of `source/generated` (`*.res`) in the unit output directory.

`G2.PreludeSnapshot.inc` holds the preludes compiled ahead of time (`G2.Snapshot`). `build.pas` makes it per program: it compiles the program, runs it once with `G2_SNAPSHOT_OUT` set, writes what the run recorded as the include and compiles again. A program compiled with an empty snapshot (`G2PreludeSnapshotSize = 0` and a one-byte `G2PreludeSnapshot`) also works and compiles its preludes each time an engine is created.

## Quick Start

```pascal
program Hello;

{$mode delphiunicode}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils,

  G2.Embed;

var
  Engine: TG2Engine;
  RunResult: TG2RunResult;
begin
  Engine := TG2Engine.Create;
  try
    RunResult := Engine.Run('hello.js', 'console.log("hello from GocciaScript");');
    if not RunResult.Ok then
    begin
      WriteLn(ErrOutput, UTF8Encode(RunResult.ErrorMessage));
      ExitCode := 1;
    end;
  finally
    Engine.Free;
  end;
end.
```

`Run` does not read a file. `FileName` names the entry for diagnostics, for detecting its goal (a `.mjs` name is a Module) and for resolving its relative imports; `Source` is what runs. A host that runs a file reads it and passes its text, as `GocciaScriptLoaderBare` does.

## Engine API

`TG2Engine` (`G2.Embed`) is a plain class. Its settings are properties read when the host is prepared or when a run starts:

| Group | Members |
|-------|---------|
| Life cycle | `Create`, `Destroy`, `PrepareHost`, `Run`, `RunArtifact`, `CompileArtifact` |
| Language | `Profile` (`TLangProfile`, from `RecommendedProfile`), `SourceKind`, `JSModuleSources` |
| Host surface | `Bare`, `ExposeTimers`, `ConfigJson`, `TestMode`, `BenchMode`, `HeapSnapshots`, `Deterministic`, `RandomRoot`, `HostEnvironment` |
| Results and output | `ResultForm`, `OnConsole`, `OnWarning`, `OnModuleWarning`, `FailOnUnhandledRejection`, `Memory` |
| Modules | `Resolver`, `ModuleContentCheck`, `ProviderGraph`, `DefineInlineVirtualModule`, `DefineManifestVirtualModules`, `DefineConfigVirtualModules`, `DefineModuleManifestVirtualModules`, `ModuleLoaded` |
| Globals | `InjectInlineGlobal`, `InjectDataGlobals`, `InjectModuleGlobals` |
| Capabilities | `Capabilities`, `ProjectRoot`, `AuditSink`, `Audit`, `FetchMaxResponseBytes` |
| Limits | `TimeoutMs`, `MaxInstructions`, `MaxMemory`, `MaxFrames` |
| Test and benchmark hosts | `RunTests`, `EndTests`, `LateRejection`, `RunBenchmarks`, `HeapSnapshotObserved`, `HeapSnapshotVerify` |
| Profiling and coverage | `ProfileOpcodes`, `ProfileFunctions`, `Coverage` |

The remaining members (`BeginGuestLoads`, `GuestScope`, `GuestReadAllowed`, `EndGuestLoads`, `HostEnvironmentGuest`, `DescribeHostError`, `ForkRandom`) serve the CLI hosts' handling of project config files and nested sandbox runs.

### Engine Constructor

`TG2Engine.Create` takes no arguments. It builds the thread's heap, VM, intrinsics and realm (`G2.Engine` `EngineInit`) and sets the defaults: the recommended language profile, `ConfigJson` `'{}'`, `ExposeTimers` on, and every other setting off or zero. `Destroy` frees the thread's engine; values and closures from it are invalid afterwards.

The host's globals and modules are installed once, by `PrepareHost(Err)` or by the first `Run`, `RunArtifact` or injection call; `CompileArtifact` needs no host. Settings that shape the host (`Bare`, `ExposeTimers`, `ConfigJson`, `TestMode`, `Deterministic`, `Capabilities`, `Profile.ShadowRealm`) must be set before that. Settings of a run (`Profile`, `SourceKind`, `ResultForm`, the callbacks, `Resolver` and the limits) are read by each `Run`, so a long-lived engine may change them between runs.

### Running a Script or a Module

`SourceKind` chooses the entry's goal:

| `SourceKind` | Entry runs as |
|--------------|---------------|
| `g2skDetect` (default) | A Module when the file name ends in `.mjs` or the source has a top-level `import` or `export` declaration, and for ambiguous top-level `await` a Module first; otherwise a Script ([entry source type policy](language.md#modules)) |
| `g2skScript` | A Script; GocciaScript's [Script imports](language.md#modules) are linked first and bound as its globals |
| `g2skModule` | A Module |

`TG2RunResult.IsModule` reports the goal the run settled on. After the entry, `Run` runs the jobs the script queued, then its timers in due order on the virtual timer clock, each followed by its jobs (`G2.HostAPI` `RunEventLoop`; [ADR 0113](adr/0113-deterministic-virtual-timer-queue.md)), and waits for its `fetch` requests. An entry Module that is still waiting once nothing is left to settle it fails with `TypeError: await: Promise did not settle`.

### Long-Lived Engine

Runs on one engine share its realm. A Script's top-level `let`, `const` and `class` declarations become global lexical bindings (ES2026 §16.1.7 GlobalDeclarationInstantiation), so a later Script sees them, and declaring one again is a `SyntaxError`. `GocciaREPL` runs every input this way with `SourceKind := g2skScript`:

```pascal
Engine.SourceKind := g2skScript;
Engine.ResultForm := g2rfString;
Show(Engine.Run('session', 'const x = add(40, retries - 1);'));  // undefined
Show(Engine.Run('session', 'x + limits.max'));                   // 52
```

(`Show` prints `ResultText`; `add`, `retries` and `limits` are injected in [Injecting Custom Globals](#injecting-custom-globals).) A Module's declarations stay in that module.

### Reading Return Values

Values live on the engine's thread and never leave it. A host reads the entry's completion value as text, by setting `ResultForm` before the run:

| `ResultForm` | `TG2RunResult.ResultText` |
|--------------|---------------------------|
| `g2rfNone` (default) | Nothing; `HasResult` stays False |
| `g2rfString` | `String(value)`, as `--print` shows it |
| `g2rfJSON` | `JSON.stringify(value)` by the built-in `JSON.stringify` taken when the host was installed; `undefined` and what JSON leaves out read `null`, a function its quoted `String` form |
| `g2rfREPL`, `g2rfREPLColor` | The REPL's display of the value, without or with ANSI colors |
| `g2rfSandbox` | An encoding another engine decodes (`G2.Sandbox` `EncodeSandboxValue`), for a nested sandbox run |

The value is converted after the event loop, under the run's limits; a conversion that throws fails the run. Data that must reach Pascal in another shape goes through a native function of the host's own ([below](#injecting-a-native-function)).

### Timing

`TG2RunResult.CompileNanoseconds` is the time spent lexing, parsing and compiling the entry, and `ExecuteNanoseconds` the time spent running it with its event loop. `FormatDuration` (`TimingUtils`) prints either.

### Upgrading from 0.14

GocciaScript 0.15.0 removed the tree-walk interpreter and folded the executor into TGocciaEngine ([ADR 0138](adr/0138-bytecode-is-the-only-way-gocciascript-executes.md)). That engine, TGocciaRuntime and the runtime extension classes are now gone as well: code written for 0.14, or for the TGocciaEngine API of later releases, moves to `TG2Engine`.

| Before (no longer exists) | Now |
|--------|-----|
| TGocciaEngine.Create(FileName, Source), Execute, RunScript | `TG2Engine.Create`, then `Run(FileName, Source)` |
| TGocciaRuntime, ApplyLoaderRuntimeProfile, runtime extensions | `PrepareHost` installs the host surface; `Bare` for the core language only ([Runtime Extensions](#runtime-extensions)) |
| Engine.SourceType (stDetect, stScript, stModule) | `SourceKind` (`g2skDetect`, `g2skScript`, `g2skModule`) |
| Compatibility, StrictTypes, WarningUnsupportedFeatures | Fields of `Profile` ([Preprocessors and Compatibility](#preprocessors-and-compatibility)) |
| GlobalBackedTopLevel | None: a Script's top-level declarations are always global bindings |
| FunctionConstructor.Enabled, EnableShadowRealm | `Profile.FunctionCtor`, `Profile.ShadowRealm` |
| EnableEval | None ([Changing the Defaults](#changing-the-defaults)) |
| GlobalScope.DefineLexicalBinding, RegisterGlobal | `InjectInlineGlobal`, `InjectDataGlobals`, `InjectModuleGlobals`, or `G2.VM` `InjectGlobalBinding` |
| InjectModule, InjectModulesFromJSON | `DefineInlineVirtualModule`, `DefineManifestVirtualModules` |
| AddAlias, LoadImportMap on the engine | The same calls on a `TGocciaModuleResolver` assigned to `Resolver` |
| The console's OutputCallback | `OnConsole` |
| StartExecutionTimeout, StartInstructionLimit, SetMaxStackDepth, TGarbageCollector.MaxBytes | `TimeoutMs`, `MaxInstructions`, `MaxFrames`, `MaxMemory` |
| UnhandledRejections | `FailOnUnhandledRejection` |
| Pascal exceptions from a run | Fields of `TG2RunResult` ([Error Handling](#error-handling)) |
| CaptureAbortHandle, Engine.FetchTransport | None ([Aborting a Run from Another Thread](#aborting-a-run-from-another-thread), [Fetch Transport](#fetch-transport)) |

## Engine Lifecycle & Realm Isolation

Each `TG2Engine` owns its thread's heap, atoms, shapes, stacks, intrinsics and job queue; `Destroy` frees all of them (`G2.Engine` `EngineDone`). The next engine on the thread starts from fresh intrinsics, so a script that changed `Array.prototype`, even with a non-configurable property, cannot affect it.

- **Test runners and conformance harnesses** create an engine per file. `GocciaTestRunner` does, on a thread per file.
- **Hosts of untrusted scripts** create an engine per script, so each starts from a clean realm.
- **REPLs and sessions** keep one engine, so a change one input makes to the realm is seen by the next.

A `ShadowRealm` (with `Profile.ShadowRealm`) is a realm of its own inside the engine.

### Threading

One engine exists per thread at a time; values never cross threads. Engines on different threads run in parallel: `GocciaRunner`, `GocciaTestRunner` and `GocciaBenchmarkRunner` run each file on a thread of its own with its own engine, and `TG2SandboxHost` runs each nested `runScript` on a new thread, passing the child's value back as text.

- The JIT compiles on one thread at a time: the thread that loaded `G2.VM`, until another thread calls `JitTakeThread` (`G2.VM`). The CLI hosts hand it to the first file thread that claims it; code on other threads runs in the interpreter.
- The VM checks the native stack (`G2.Core` `NativeStackLow`) and throws `RangeError: Maximum call stack size exceeded` before it runs out, whatever the thread's stack size. The CLI hosts give a file thread a 64 MiB stack (`FILE_THREAD_STACK`), so native recursion reaches as deep as on the main thread.

## Preprocessors and Compatibility

`Profile` is a `TLangProfile` (`G2.Core`). `RecommendedProfile` returns the recommended profile: `Restricted` set and every switch off. A host changes switches on a copy and assigns it before the run:

```pascal
Profile := RecommendedProfile;
Profile.WarningUnsupported := True;
Engine.Profile := Profile;
```

| Field | Turns on | CLI equivalent |
|-------|----------|----------------|
| `Asi` | Automatic semicolon insertion (ES2026 §12.10) | `--compat-asi` |
| `VarDecl`, `FuncDecl` | `var` declarations, `function` declarations | `--compat-var`, `--compat-function` |
| `ForLoop`, `WhileLoop`, `ForIn`, `Labels` | `for(;;)`, `while` and `do...while`, `for...in`, labeled statements | `--compat-traditional-for-loop`, `--compat-while-loops`, `--compat-for-in-loop`, `--compat-label` |
| `LooseEq` | `==` and `!=` | `--compat-loose-equality` |
| `NonStrict`, `ArgumentsObject` | Sloppy-mode code, the `arguments` object | `--compat-non-strict-mode`, `--compat-arguments-object` |
| `StrictTypes` | Runtime checks of type annotations | `--strict-types` |
| `WarningUnsupported` | Warn about unsupported or disabled syntax and recover, instead of a `SyntaxError` | `--warning-unsupported-features` |
| `FunctionCtor` | The `Function` constructor | `--unsafe-function-constructor` |
| `ShadowRealm` | The `ShadowRealm` global (read when the host is installed) | `--unsafe-shadowrealm` |

With `Restricted` False every compatibility switch reads as on (`G2.Core` `ProfileNonStrict` and the rest); the CLI hosts keep it set. `JSModuleSources` turns on GocciaScript's experimental JavaScript module source objects (`--experimental-js-module-source`). JSX, TypeScript-style type annotations and the rest of the front end's preprocessing apply as the [Language](language.md) page describes; G2 uses the shared front end (`Goccia.SourcePipeline`) with these options.

## Changing the Defaults

An engine starts with the recommended language profile and without dynamic code generation. Three settings change that, each off by default:

| Setting | What it turns on | CLI | Audit event |
|---------|------------------|-----|-------------|
| `Profile` compatibility fields | The ECMAScript forms the recommended profile leaves out ([table above](#preprocessors-and-compatibility)) | `--compat-*` | none |
| `Profile.FunctionCtor` | The `Function` constructor, which throws while it is off | `--unsafe-function-constructor` | `function.constructor` |
| `Profile.ShadowRealm` | Installs `globalThis.ShadowRealm` | `--unsafe-shadowrealm` | `shadow-realm.construct` |

A `TG2Engine` host has no global `eval`: the host deletes it from every realm it creates (`G2.HostAPI` `CLIRealmInit`), and no setting installs it. The `Function` constructor and `ShadowRealm.prototype.evaluate` run any source text the script builds, with the engine's full capability set, so a host that runs untrusted code leaves them off.

## Runtime Extensions

G2 has no runtime extension classes. Its host surface is a set of JS preludes (`source/g2/prelude`) that `PrepareHost` installs with the natives they need (`G2.HostAPI` `InstallHostGlobals`):

- **Globals**: `console`, `performance`, the timer functions, `queueMicrotask`, `URL` and `URLSearchParams`, `fetch` with `Headers` and `Response`, `AbortController`, `structuredClone`, `TextEncoder` and `TextDecoder`, `atob` and `btoa`, `DOMException`, `PermissionDenied`, and the `Goccia` object.
- **Modules**: `goccia:csv`, `goccia:tsv`, `goccia:json5`, `goccia:jsonl`, `goccia:toml`, `goccia:yaml`, `goccia:semver`, `goccia:timers` and `node:async_hooks`; `goccia:ast` when `ConfigJson` sets `"experimental-ast": "true"`.

Settings choose the rest:

| Setting | Effect |
|---------|--------|
| `Bare` | Only the core language with `print`, `Goccia`, `DOMException`, `atob`, `btoa` and `PermissionDenied`: no console, timers, web APIs or host modules, and no module content provider, so an import that finds its file fails with `ERR_MODULE_LOADING_UNSUPPORTED` (`GocciaScriptLoaderBare`) |
| `ExposeTimers := False` | No timer globals and no `goccia:timers`; the timer queue still serves the host's own users, such as `AbortSignal.timeout` |
| `ConfigJson` | The host config as a JSON object of strings (`goccia.json`'s keys), read by the preludes |
| `TestMode` | The test framework's host: an uncaught error does not end the process; `InstallTestingAPI` (`G2.HostAPI`) then installs `describe`, `test`, `expect` and `goccia:test`, and `RunTests` runs the suites |
| `BenchMode` | The benchmark runner's host: `InstallBenchmarkAPI` installs `goccia:microbench`, and the limits `Run` armed stay armed for `RunBenchmarks` |
| `Capabilities` granting `ffi` | Installs the `FFI` global; `FFI.open` judges each library by the `ffi` scopes |

### Sandbox filesystem

`TG2SandboxHost` (`Goccia.CLI.G2SandboxHost`) runs a program over an in-memory filesystem, as `GocciaRunner`'s sandbox mode does: each run gets a thread and an engine of its own, imports and data files come from the virtual filesystem, and scripts see the import-only `"fs"`, `"goccia"` and `goccia:search` [sandbox modules](built-ins.md#sandbox-modules). The embedder fills `Host.Context.Fs` (a `TSandboxVirtualFileSystem`) before the run.

```pascal
Host := TG2SandboxHost.Create(DEFAULT_SANDBOX_BYTE_QUOTA, DEFAULT_SANDBOX_NODE_QUOTA);
try
  Host.TimeoutMilliseconds := 5000;
  Host.Context.Fs.MakeDirectory('/data', True);
  Host.Context.Fs.WriteAllText('/data/input.json', '{"items": [1, 2, 3]}');
  Host.Context.Fs.MakeDirectory('/out');
  Host.Context.Fs.AddReadOnlyPath('/data');
  Host.Context.Fs.WriteAllText('/main.js',
    'import fs from "fs";' + LineEnding +
    'const input = JSON.parse(fs.readFileSync("/data/input.json", "utf8"));' + LineEnding +
    'fs.writeFileSync("/out/sum.txt", String(input.items.reduce((a, b) => a + b, 0)));');
  Host.CaptureBaseline;
  RunResult := Host.Run('/main.js');
  // RunResult.Ok, RunResult.FailureKind, RunResult.Output (the console's lines)
  WriteLn(Host.Context.Fs.ReadAllText('/out/sum.txt'));  // 6
  WriteLn(Host.DiffText(True));                           // unified diff against the baseline
finally
  Host.Free;
end;
```

`AddReadOnlyPath` makes a path and everything under it read-only, whether or not it exists yet: an `fs` call that would change something there fails with an `Error` whose `code` is `"EROFS"` ([Sandbox filesystem errors](errors.md#sandbox-filesystem-errors), [ADR 0144](adr/0144-read-only-paths-are-enforced-by-the-virtual-filesystem.md)). Mark paths after filling them, because the marks refuse the embedder's writes too. `RootCapabilities` is the root run's capability set (`TGocciaCapabilities.None` by default); a nested `runScript` inherits its parent's set and at most the time its parent has left. The `FailureKind` of a `TGocciaSandboxRunResult` tells a script error from a resource ceiling or a timeout (`SandboxFailureKindName`). `OnConfigureEngine` sets up each run's `TG2Engine` (profile, settings, audit) before it runs.

The third constructor argument is an optional `TSandboxFsClock` that stamps every file of the host's filesystem and of every isolated nested run; `nil` reads the wall clock, and the host does not own it. `TGocciaSandboxFsHostClock.Create(TGocciaHostEnvironment.DeterministicClock)` stamps every file with the epoch, in step with an engine that uses the [deterministic profile](host-environment.md#deterministic-profile); `GocciaRunner --deterministic` does this in sandbox mode.

## Console Output Capture

Without `OnConsole`, console lines go to standard output. `OnConsole` (a `TG2ConsoleEvent`, `G2.HostAPI` `TConsoleSink`) receives each line with the console method that wrote it instead. `Line` is the text the console would print, including the prefix the console gives some methods, such as `Warning:` for `console.warn`:

```pascal
type
  TMyHost = class
  public
    procedure Console(const Method, Line: UnicodeString);
  end;

procedure TMyHost.Console(const Method, Line: UnicodeString);
begin
  WriteLn('[', UTF8Encode(Method), '] ', UTF8Encode(Line));
end;

Engine.OnConsole := Host.Console;
```

`GocciaRunner` captures the console this way for its JSON output (`Goccia.CLI.G2FileEngine` `TG2ConsoleCapture`), and `TG2SandboxHost` collects each run's lines in its result's `Output`.

## Parse Warnings

The front end reports what it recovered from: unsupported or disabled syntax when `Profile.WarningUnsupported` is set, and JSX in a file whose extension is not a JSX one. `OnWarning` receives the entry's warnings before it runs; `OnModuleWarning` receives an imported module's as the module compiles, from the run and from the host's own loads (virtual modules, globals modules). Set `OnModuleWarning` on the engine's thread.

```pascal
procedure TMyHost.Warning(const Message, Suggestion: UnicodeString; Line, Column: Integer);
begin
  WriteLn(ErrOutput, 'warning ', Line, ':', Column, ' ', UTF8Encode(Message));
end;

procedure TMyHost.ModuleWarning(const FileName, Message, Suggestion: UnicodeString; Line, Column: Integer);
begin
  WriteLn(ErrOutput, UTF8Encode(FileName), ':', Line, ':', Column, ' warning: ', UTF8Encode(Message));
end;

Engine.OnWarning := Host.Warning;
Engine.OnModuleWarning := Host.ModuleWarning;
```

## Injecting Custom Globals

Prefer [virtual modules](#virtual-modules) for host-provided dependencies; inject globals when code must consume an implicit binding. Each injected global is a `const` global lexical binding, mirrored as a non-enumerable property of the global object. A name a built-in or a script already binds is refused; a name injected before takes the new value. The injection calls install the host first if it is not installed, and report failure through their `Err` argument:

```pascal
Err := '';
if not Engine.PrepareHost(Err) or
   not Engine.InjectInlineGlobal('appName', 'demo', Err) or
   not Engine.InjectInlineGlobal('retries', '3', Err) or
   not Engine.InjectDataGlobals('{"limits": {"max": 10}}', '.json', 'settings.json', Err) then
  WriteLn(ErrOutput, 'setup failed: ', UTF8Encode(Err));
```

| Call | Injects |
|------|---------|
| `InjectInlineGlobal(Name, ValueText, Err)` | One global: `ValueText` read as JSON, or as that string when it is not JSON (`--global name=value`) |
| `InjectDataGlobals(Text, Ext, Path, Err)` | Each own enumerable property of the object `Text` parses to, by the parser for `Ext` (`.json`, `.json5`, `.jsonc`, `.toml`, `.yaml`, `.yml`) |
| `InjectModuleGlobals(Path, Err)` | Each named export of the JavaScript or TypeScript module at `Path`, evaluated in this engine |

`InjectInlineGlobal('Math', '1', Err)` fails with `Cannot override built-in global "Math" via globals injection.`

### Injecting a Native Function

`TG2Engine` has no method for Pascal functions. A host defines one with the `G2.VM` routines the host's own natives use (`G2.HostAPI`), after `PrepareHost`, on the engine's thread. A native is a `TNativeFn` (`G2.Core`): `Args[0]` is `this`, `Args[1..Argc]` the arguments; it writes its result and returns True, or returns False with an exception pending. The function value must stay rooted while anything allocates:

```pascal
{$POINTERMATH ON}

uses
  G2.Core,
  G2.Embed,
  G2.VM;

{ add(a, b) }
function NativeAdd(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var
  A, B: Double;
begin
  if Argc < 2 then
    Exit(ThrowTypeError('add expects two arguments'));
  if not ToNumberV(Args[1], A) or not ToNumberV(Args[2], B) then
    Exit(False);
  Res := MkNumber(A + B);
  Result := True;
end;

function DefineHostFunction(const Name: UnicodeString; Fn: TNativeFn; Length: Integer): Boolean;
begin
  PushRoot(NewNativeFunction(Fn, Name, Length));
  try
    Result := InjectGlobalBinding(Name, RootAt(TempTop - 1));
  finally
    PopRoots(1);
  end;
  if not Result then
    Pending := VUndefined;  // the TypeError a refused name leaves pending
end;
```

`DefineHostFunction('add', @NativeAdd, 2)` then makes `add(40, 2)` return 42, and `add(1)` throws the `TypeError`. `ToStringV`, `StrFromUnicode` and `StrToUnicode` convert strings; `NewNativeFunctionData` attaches a 32-bit value a native reads back. The capability set governs only what the engine does itself; a native is host code the engine does not check, so it authorizes what it reaches itself.

## Module Resolution

`Resolver` is the run's import resolution (a `TModuleResolver`, normally a `TGocciaModuleResolver` from `Goccia.Modules.Resolver`); the engine does not own it. With none, imports resolve relative and absolute paths only, trying the extensions of `ModuleImportExtensions` (`Goccia.FileExtensions`), and a bare specifier is refused. A resolver of the host's own starts with no extensions:

```pascal
Resolver := TGocciaModuleResolver.Create(Root);
Resolver.SetExtensions(ModuleImportExtensions);
Resolver.AddAlias('@/', 'src/');
Engine.Resolver := Resolver;
```

Aliases follow WHATWG import map matching: a key without a trailing `/` matches exactly, a key with one is a prefix, and the longest matching prefix wins. Relative alias targets resolve against the resolver's base directory. `LoadImportMap(Path)` reads an import map, and `DiscoverProjectConfig(StartDirectory)` finds a `goccia.json`. The CLI hosts configure their resolver with `ConfigureModuleResolver` (`Goccia.Modules.Configuration`); see [Module Resolution](module-resolution.md) and [Configuration File](build-system.md#configuration-file-gocciajson).

### Bare Specifiers and node_modules

A bare specifier walks `node_modules` only when the resolver's `NodeModulesGrant` callback grants the walk; a resolver without one refuses it. The CLI hosts answer it from the `import` capability (`Goccia.CLI.G2FileEngine` `TG2ImportGrant`), which also grants and audits provider packages (`ProviderGrant`, `ProviderAudit`) and checks their files against their pins through `ModuleContentCheck` and `ProviderGraph`.

### Virtual Modules

Virtual modules supply modules that are not files ([Virtual Module Configuration](virtual-modules.md)). Define them before the run:

```pascal
if not Engine.DefineInlineVirtualModule('host:config=export const answer = 41;', UnicodeString(Root), Err) then
  WriteLn(ErrOutput, 'virtual module: ', UTF8Encode(Err));
```

A definition is `address=source` or `address=` followed by a JSON descriptor; relative addresses resolve from the base directory. `DefineManifestVirtualModules(Path, Err)` reads a module map in JSON, JSON5, TOML or YAML, `DefineModuleManifestVirtualModules(Path, Err)` takes the default export of a JavaScript or TypeScript module, and `DefineConfigVirtualModules(ConfigPath, Err)` applies a config file's `"modules"` and `"module"` keys.

### Custom Content Provider

`TG2Engine` reads module files from the host filesystem through `Resolver`'s result, and has no content-provider property. `ModuleContentCheck` sees each module file's bytes before they are compiled and refuses a file by raising. For module text that is not on disk, use virtual modules; to serve a whole tree from memory, use the [sandbox filesystem](#sandbox-filesystem), which installs a thread file reader (`G2.Modules` `SetFileReadHook`) and a resolver over the same tree (`TGocciaSandboxModuleResolver`). With `Bare` set there is no content provider at all ([Errors](errors.md)).

### Bytecode Files

`CompileArtifact(FileName, Source, Bytes, R)` compiles a source as `Run` would and writes it as a G2 bytecode file (`G2.Artifact`) without running it; `RunArtifact(FileName, Bytes)` runs such a file as the entry after checking its recorded profile against the engine's settings. `GocciaBundler` and `GocciaRunner` use them; see [Precompiled bytecode modules](bytecode-loading.md).

## Capabilities and Audit

`Capabilities` is the run's `TGocciaCapabilities` (`Goccia.Capabilities`, [Permissions](permissions.md), [ADR 0122](adr/0122-unified-capability-model.md)). Set it before the host is prepared:

```pascal
Engine.Capabilities := TGocciaCapabilities.None.Allow(gcFFI, '/usr/lib').Allow(gcRead, '/etc');
Engine.AuditSink := Log.Record_;
```

| Capability | With `Capabilities` set | Without |
|------------|------------------------|---------|
| `read` | A literal import inside `ProjectRoot` needs no grant; a computed import (`import(variable)`) and anything outside need a `read` grant, else `PermissionDenied` | Module reads are not judged |
| `net` | `fetch` checks each destination, before the lookup and against the address it resolved to | `fetch` is judged by `TGocciaCapabilities.None`, so every request is refused |
| `ffi` | The `FFI` global is installed; `FFI.open` judges each library path | No `FFI` |
| `import` | Answered by the resolver's callbacks ([above](#bare-specifiers-and-node_modules)) | The same |

`ProjectRoot` defaults to the directory of the nearest `goccia.json`, `goccia.json5` or `goccia.toml` above the entry, else the entry's own directory (`DiscoverProjectRoot`). `FetchMaxResponseBytes` caps a response body (0: 8 MiB).

`AuditSink` (a `TGocciaCapabilityAuditSink`, `Goccia.CapabilityAudit`) receives every decision the run makes, at the script's call site: the `Function` constructor's, a `ShadowRealm`'s, an `FFI.open`'s, a `fetch`'s and the read policy's ([Capability Audit](capability-audit.md)). `TGocciaCapabilityAuditEvent.ToJSON` gives the audit log's line:

```json
{"schemaVersion":1,"kind":"net.fetch","decision":"deny","subject":"example.com","reason":"the net capability does not allow this host","source":{"file":"/app/caps.js","line":3,"column":13}}
```

`Audit(Kind, Allow, Subject, Reason)` reports a decision of the host's own the same way. A sink that raises ends the run past every handler, with `Aborted = g2aHostAbort`.

## Error Handling

`Run` reports how a run ended in its `TG2RunResult`; a script error, a syntax error or a spent limit never raises a Pascal exception. See [Errors](errors.md) for the error types and their display, and [JSON Output](json-output.md) for the CLI's report.

| Field | Meaning |
|-------|---------|
| `Ok` | The run completed |
| `Compiled` | The entry compiled; False for a syntax error |
| `Aborted` | `g2aNone`, or the limit or host abort that ended the run ([Execution Limits](#execution-limits)) |
| `ErrorMessage` | The uncaught value as text (`TypeError: bad input`), or the limit's message |
| `ErrorIsError` | The thrown value had a string `name` or `message`, or the source did not compile |
| `ErrorName`, `ErrorText` | Its name and message |
| `ErrorFile`, `ErrorLine`, `ErrorColumn` | Where it was made: the first frame of its stack, or where the source was rejected (`ErrorLine` 0 when unknown) |
| `ErrorSuggestion` | The parser's or the host's suggestion |
| `ErrorResolvedPath` | For a static import that failed to load, the path its resolution tried |

```pascal
if not R.Ok then
  if not R.Compiled then
    WriteLn('syntax error at ', R.ErrorLine, ':', R.ErrorColumn, ': ', UTF8Encode(R.ErrorText))
  else if R.Aborted <> g2aNone then
    WriteLn('stopped: ', UTF8Encode(R.ErrorMessage))
  else if R.ErrorIsError then
    WriteLn(UTF8Encode(R.ErrorName), ': ', UTF8Encode(R.ErrorText), ' (', UTF8Encode(R.ErrorFile), ':',
      R.ErrorLine, ':', R.ErrorColumn, ')')
  else
    WriteLn('uncaught: ', UTF8Encode(R.ErrorMessage));
```

`FailureText` (`Goccia.CLI.G2FileEngine`) formats a failed run as `GocciaRunner` prints it, with a code frame.

**Unhandled rejections.** A promise left rejected with no handler (ES2026 §27.2.1.9 HostPromiseRejectionTracker) fails the run only when `FailOnUnhandledRejection` is set; its reason is then the uncaught error. It is off by default, so an embedder that wants the CLI behaviour ([ADR 0124](adr/0124-unhandled-rejections-fail-the-run.md)) sets it, as `GocciaRunner`, `GocciaREPL` and `TG2SandboxHost` do. What a run left rejected is cleared with it, so the next run on the engine is not blamed.

**Host exceptions.** Pascal exceptions can still reach the host from host-level failures, such as a constructor that cannot build the engine (see [Building an Embedding](#building-an-embedding)).

## Execution Limits

Four limits bound a run. Each is a property read by `Run`; 0 means none (`MaxMemory` 0: the default ceiling). A run with a timeout, an instruction budget or a frame bound runs without the JIT, since compiled code does not poll a deadline, and so does a run with `Coverage` or a profile; the memory limit leaves the JIT on.

```pascal
Engine.TimeoutMs := 2000;
Engine.MaxInstructions := 100000000;
Engine.MaxMemory := 256 * 1024 * 1024;
Engine.MaxFrames := 5000;
```

### Timeout

`TimeoutMs` arms a timer thread that interrupts the engine thread at the deadline (`G2.VM` `LimitsArm`). The run ends past every handler with `Aborted = g2aTimeout` and `Execution timed out after Nms`. The deadline covers the entry, its event loop and, in test and benchmark mode, the suites.

### Instruction Limit

`MaxInstructions` counts dispatched instructions; a native loop whose trip count the script controls charges one step per poll (`PollInterrupt`). The run ends with `Aborted = g2aInstructionLimit` and `Execution exceeded instruction limit of N`.

### Memory Limit

`MaxMemory` sets the thread's heap ceiling (`G2.Core` `HeapCeiling`, per thread). Without it the ceiling is half the physical memory, at most 8 GB, or 700 MB on a 32-bit build (`DefaultHeapCeiling`). A full collection that leaves the heap over the ceiling refuses at the next back edge or call with a catchable `RangeError` (`Allocation failed: GC heap size exceeds the configured memory limit`), so a script can drop what it holds and go on. When refusals keep coming without the script freeing memory, the run ends with `Aborted = g2aMemoryLimit` and `ErrorText` says how much was live ([ADR 0185](adr/0185-g2-refuses-memory-at-the-next-safe-point.md)). `Memory` returns a `TG2MemoryStats`: live, peak and allocated bytes, the limit, and the number of collections.

### Aborting a Run from Another Thread

`TG2Engine` has no abort handle: another thread cannot stop a run. A host bounds the run with `TimeoutMs`, whose timer thread is the only cross-thread stop. Code the JIT compiled does not poll a deadline, which is why the limits keep the JIT off; `GocciaFuzzHarness` ends its JIT run, which has no limits, with a watchdog that exits the process.

### Call Stack Depth Limit

`MaxFrames` bounds the nested calls of a run (`G2.VM` `SetMaxFrames`); past it a call throws `RangeError: Maximum call stack size exceeded`. With 0 the bound is the VM's frame stack, about 200,000 frames (`FRAME_MAX`). Calls in tail position (ES2026 §15.10.3 PrepareForTailCall) reuse their frame once the stack is past a depth floor, so tail recursion runs in bounded space and ends only at another limit ([ADR 0187](adr/0187-g2-reuses-a-frame-for-a-tail-call-only-past-a-depth-floor.md)). Independently of `MaxFrames`, native recursion (Proxy traps, bound function chains, re-entries from built-ins) throws the same `RangeError` once the thread's native stack is nearly used up ([Threading](#threading), [ADR 0182](adr/0182-linux-thread-stack-bounds-come-from-pthread-getattr-np.md)).

## Fetch Transport

`TG2Engine` has no fetch transport. `fetch` (`G2.Fetch`) performs each request on a worker thread with the shared network client (`HTTPClient`), at most 16 of a thread's requests in flight, and checks the `net` capability on every hop before the name is looked up and again against the resolved address. A host that must keep scripts off the network leaves `net` ungranted; `fetch` then rejects with `PermissionDenied`. See [Fetch](built-ins-fetch.md).

## FPU Exception Mask

`TG2Engine.Create` masks every FPU exception on the calling thread (`G2.Engine` `EngineInit`), so arithmetic yields `NaN`, `Infinity` and `-0` as IEEE 754 requires instead of raising. The mask is per thread and is not restored when the engine is freed; a host that relies on FPU exceptions on that thread sets its own mask again.

## Event Loop

Promise jobs, timers and `fetch` completions are run by `Run` itself; a host drains nothing. The script is one task: its synchronous code runs to completion, then its jobs in order, then each due timer followed by the jobs it queued. Timers wait on a virtual clock, not on wall time: when the engine runs out of work the clock jumps to the next due timer. A pending `fetch` is waited for in slices, so the run's deadline still ends the wait.

## Garbage Collector

Each engine has a generational heap (`G2.Core`): a nursery of 8 MiB by default (`EngineNurseryBytes`), collected by `CollectMinor`, and an old generation collected by `CollectMajor`. Collection is automatic. Pascal code that holds a `TValue` across anything that allocates roots it with `PushRoot` and reads it back with `RootAt`, since a collection may move or free an unrooted value; `PopRoots` releases it. `AddRoot` roots a variable for the life of the engine.

## Existing Embeddings

Every application in `source/app` embeds the engine through `TG2Engine`, except `GocciaTest262Runner`, which runs each test in a forked process on G2's conformance host (`G2.Test262`):

| Program | File | How it embeds |
|---------|------|---------------|
| `GocciaRunner` | `source/app/GocciaRunner.dpr` | Each entry file on a thread with its own engine: settings, capabilities, audit, globals, virtual modules, `ResultForm` for `--print` and JSON, `RunArtifact` for `.gbc` entries; sandbox mode through `TG2SandboxHost` |
| `GocciaScriptLoaderBare` | `source/app/GocciaScriptLoaderBare.dpr` | The smallest host: `Bare`, a profile from compatibility flags, the limits, `OnWarning` |
| `GocciaREPL` | `source/app/GocciaREPL.dpr` | One engine for the session on its own thread, each input a Script run, `ResultForm := g2rfREPL` |
| `GocciaTestRunner` | `source/app/GocciaTestRunner.dpr` | `TestMode`, `InstallTestingAPI`, `RunTests`, heap snapshot checks |
| `GocciaBenchmarkRunner` | `source/app/GocciaBenchmarkRunner.dpr` | `BenchMode`, `InstallBenchmarkAPI`, `RunBenchmarks` |
| `GocciaBundler` | `source/app/GocciaBundler.dpr` | `CompileArtifact` on a thread per file, nothing run |
| `GocciaFuzzHarness` | `source/app/GocciaFuzzHarness.dpr` | Two fresh engines per input, one under every limit and one with the JIT |

`Goccia.CLI.G2FileEngine` holds what the file-running hosts share (settings, globals, virtual modules, console capture, the import grant, failure text), and `source/g2/g2.dpr` is G2's own development and conformance CLI.

## Application Base Class

`TGocciaApplication` (`Goccia.Application`) gives a host application the standard lifecycle without a CLI dependency: `RunApplication` creates it, calls `Execute`, routes an exception to the virtual `HandleError`, and returns the exit code (0 success, 1 error, 2 for a `TCLIUsageError`).

```pascal
type
  TMyApp = class(TGocciaApplication)
  protected
    procedure Execute; override;
  end;

procedure TMyApp.Execute;
begin
  // create a TG2Engine and run scripts here
end;

begin
  ExitCode := TGocciaApplication.RunApplication(TMyApp, 'MyApp');
end.
```

For CLI tools, `TGocciaCLIApplication` (`Goccia.CLI.Application`) adds argument parsing, help, config files, the trust gate, the capability policy and the audit log; `GocciaRunner`, `GocciaREPL`, `GocciaTestRunner`, `GocciaBenchmarkRunner` and `GocciaBundler` build on it.

## Minimal Embedding Checklist

1. Compile with FPC 3.3.1 and the unit and include paths of [Building an Embedding](#building-an-embedding)
2. `uses G2.Embed;` (add `G2.Core` for `TLangProfile`, `Goccia.Capabilities` for capability sets)
3. Create a `TG2Engine` on the thread that will run it, one per thread at a time
4. Set the host surface (`Bare`, `ExposeTimers`, `ConfigJson`, `Capabilities`) before `PrepareHost` or the first run
5. Set the run's settings: `Profile`, `SourceKind`, `ResultForm`, `OnConsole`, `Resolver`, the limits
6. Inject globals and virtual modules, then `Run(FileName, Source)` with the source text
7. Read `TG2RunResult`: `Ok`, `Aborted`, the error fields, `ResultText`
8. Free the engine on the same thread; free the resolver after it
