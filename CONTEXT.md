# GocciaScript Context

GocciaScript is a FreePascal implementation of ECMAScript with sandbox-first recommended defaults and opt-in compatibility/runtime surfaces. This glossary is the canonical language for project-specific terms used in code, docs, issues, and reviews.

## Language

### Project And Layers

**GocciaScript**:
A JavaScript-family language runtime and toolchain focused on safer ECMAScript defaults for embedding and agent execution, while keeping conformance and host customization explicit.
_Avoid_: JS engine when the recommended defaults or host-tooling shape matters.

**Recommended defaults**:
The recommended out-of-the-box way to use GocciaScript: sandbox-first, modern, and intentionally conservative about legacy JavaScript forms. It describes guidance for new code, not the limit of what the engine or runtime can support when a host opts into compatibility or custom runtime capabilities.
_Avoid_: Language ceiling, hard limit.

**Engine**:
The core language execution layer. It owns language semantics, source type, execution mode dispatch, and core language built-ins.
_Avoid_: Runtime when referring only to core language behavior.

**Runtime**:
The host integration layer attached to an engine. It installs optional globals, file-backed helpers, and runtime extensions.
_Avoid_: Engine, VM.

**Runtime extension**:
An installable host or special-purpose feature added to a runtime, such as console, fetch, data-format globals, testing, benchmarking, or FFI.
_Avoid_: Core language built-in when the feature belongs to the optional runtime surface.

**Runtime profile**:
A named bundle of runtime extensions used by a CLI host or embedding host.
_Avoid_: Mode, preset.

**Runtime surface**:
The aggregate JavaScript-visible capability set installed by a runtime profile or by individual runtime extensions. Use it when discussing what a host exposes overall; use runtime global, runtime extension, or runtime profile when naming the concrete mechanism.
_Avoid_: Runtime, built-in surface, host surface.

**Built-in**:
An API supplied by GocciaScript rather than by user source or a host application. Built-ins include both core language built-ins and runtime globals.
_Avoid_: Native function when the implementation mechanism is the point.

**Core language built-in**:
An always-available global object, function, constructor, or constant registered by the engine as part of the language core.
_Avoid_: Runtime global.

**Runtime global**:
An optional global object, function, constructor, or namespace installed by a runtime extension.
_Avoid_: Core language built-in, runtime surface when referring to one global.

**Shim**:
A GocciaScript-provided legacy ECMAScript surface layered over a newer native capability so conformance can include old names without making them the recommended path for new code.
_Avoid_: Polyfill, compatibility flag.

### Execution

**Execution mode**:
The selected way to execute source: interpreter mode or bytecode mode.
_Avoid_: Runtime, runtime profile.

**Executor**:
The implementation object behind an execution mode. GocciaScript has an interpreter executor and a bytecode executor.
_Avoid_: Execution backend, runtime.

**Source pipeline**:
The shared source-processing pipeline that turns source text into an AST before execution. It owns parser policy and exposes purpose-specific parse entry points for full source, module source, dynamic Function parsing, and expression fragments.
_Avoid_: Source frontend, CLI host when discussing lexer/parser/AST behavior.

**Parser policy**:
The source-pipeline settings that determine how source text is parsed, including source type and the compatibility flag set.
_Avoid_: Separate parser booleans when discussing the policy as a whole.

**CLI host**:
A command-line program that hosts the engine or runtime for a specific workflow.
_Avoid_: CLI frontend, source pipeline.

**CLI option**:
A named command-line control such as `--mode=bytecode`, `--output=json`, or `--print`. Use it as the umbrella term for named CLI controls, whether they take a value or not.
_Avoid_: CLI flag when the option takes a value.

**CLI flag**:
A boolean CLI option that toggles behavior by its presence, such as `--print`, `--compat-asi`, or `--unsafe-ffi`.
_Avoid_: CLI flag for value-taking controls.

**Positional argument**:
A non-option CLI input whose meaning comes from position, such as an entry file, test path, benchmark path, or `-` stdin marker.
_Avoid_: CLI option, CLI flag.

**Entry file**:
The initial script or module file path that starts a single program run. It anchors entry-specific behavior such as script-vs-module source type, root config discovery, and default relative module resolution.
_Avoid_: Input file when only the initial program path matters.

**Input file**:
Any file processed by a CLI host, especially batch-capable tools such as the Test Runner, Benchmark Runner, Bundler, or multifile processing.
_Avoid_: Entry file when referring to a batch member or per-file result.

**Config key**:
A field name in a GocciaScript config file, such as `"mode"`, `"strict-types"`, or `"allowed-hosts"`.
_Avoid_: Config option when referring to the field name.

**Config value**:
The value assigned to a config key and the selected setting it carries, such as `"bytecode"`, `true`, or `["api.example.com"]`.
_Avoid_: Config option when referring to the serialized value.

**Tree-walk execution**:
Execution by walking the AST through the interpreter and evaluator.
_Avoid_: Bytecode execution.

**Bytecode execution**:
Execution by compiling the AST to Goccia bytecode and running it on the Goccia VM.
_Avoid_: VM mode.

**Goccia bytecode**:
The GocciaScript-owned instruction format used by bytecode execution and stored in `.gbc` artifacts.
_Avoid_: Generic VM bytecode.

**Goccia VM**:
The virtual machine that executes Goccia bytecode for GocciaScript.
_Avoid_: Generic VM layer.

**Inline cache**:
A per-site, version-validated cache on a function template that lets the Goccia VM re-read a previously resolved global binding or own data property by entry index instead of by name. A site whose receivers keep changing becomes megamorphic and reads through the uncached fast path instead.
_Avoid_: Shape cache, hidden class — GocciaScript inline caches validate map identity and entry version, not object shapes.

### Source And Tools

**Source text**:
The textual contents of GocciaScript code, independent of whether it came from a file, stdin, or an embedding host.
_Avoid_: Source file when referring to in-memory contents.

**Preprocessor**:
A source-to-source transformation applied before the source pipeline lexes and parses source text. JSX is the current preprocessor.
_Avoid_: JSX flag when referring to the general mechanism.

**Source type**:
The setting that chooses whether an entry file is evaluated as script source or module source. It may be explicit or inferred from the entry file name.
_Avoid_: Mode, module mode, script mode.

**Script source**:
Source evaluated with script entry semantics.
_Avoid_: Module source.

**Module source**:
Source evaluated with module entry semantics, including module `this`, imports, exports, and import metadata.
_Avoid_: Script source.

**Script Loader**:
The CLI host that executes source files, stdin, or `.gbc` artifacts.
_Avoid_: Script executor.

**Bare Script Loader**:
The CLI host that executes through the core engine without attaching the runtime surface.
_Avoid_: Loader profile.

**Test262 host capability**:
A JavaScript-visible hook exposed on the `Goccia` namespace only when a CLI host opts into the test262 conformance contract, such as `GocciaScriptLoaderBare --test262-host`. It is not a core language built-in and not part of the normal runtime surface.
_Avoid_: Runtime global, compatibility flag.

**Bundler**:
The CLI host that compiles source to `.gbc` artifacts without executing the program.
_Avoid_: Compiler when referring to the user-facing tool.

### Values And Bindings

**Value**:
The runtime representation of JavaScript data in GocciaScript.
_Avoid_: Object when primitives are included.

**Literal**:
Source syntax that directly denotes a value.
_Avoid_: Runtime value when discussing stored or computed data.

**Scope**:
The lexical context used to resolve bindings and contextual values such as `this`, `super`, and `new.target`.
_Avoid_: Object, namespace.

**Binding**:
A name-to-value association in a scope.
_Avoid_: Raw variable.

**Define**:
Create a new binding in the current scope.
_Avoid_: Assign.

**Assign**:
Change the value of an existing binding.
_Avoid_: Define.

**Realm**:
The ECMAScript execution domain for code: its own intrinsics, global object, global environment, loaded code state, and host-associated resources.
_Avoid_: Runtime, process-wide singleton, prototype cache.

### Functions And Objects

**Native function**:
A GocciaScript-callable function whose body is implemented in Pascal. It may come from the engine, a runtime extension, or an embedding host.
_Avoid_: Built-in when the function is host-provided rather than language-provided.

**User-defined function**:
A function whose body is written in GocciaScript source. This includes arrow functions and opt-in compatibility functions.
_Avoid_: User function, if arrow-only behavior is not intended.

**Arrow function**:
A source-level `=>` function with lexical `this`.
_Avoid_: User-defined function when lexical `this` is the important distinction.

**Ordinary function**:
A non-arrow source-defined function with call-site `this` semantics.
_Avoid_: Arrow function, method.

**Method**:
A class or object shorthand function that receives its call-site receiver as `this`.
_Avoid_: Arrow method.

**Compatibility flag**:
An explicit boolean CLI flag or config value, using canonical `compat-*` spelling, that enables an excluded ECMAScript compatibility behavior.
_Avoid_: Feature flag when the option exists for compatibility semantics.

**Compatibility flag set**:
The aggregate source-pipeline setting that carries enabled compatibility flags together, such as `compat-asi`, `compat-var`, `compat-function`, or `compat-non-strict-mode`.
_Avoid_: Separate compatibility booleans when a caller is passing the whole parser compatibility policy.

## Flagged Ambiguities

**Runtime**:
Use **Runtime** for the host integration layer. Use **runtime value**, **runtime behavior**, or **runtime error** when speaking generically about behavior during execution.

**Executor vs mode**:
Use **execution mode** for the user-visible selection. Use **executor** for the implementation object that makes the mode work.

**Option vs flag**:
Use **CLI option** as the umbrella term. Use **CLI flag** only for boolean options; call value-taking controls such as `--mode=bytecode`, `--output=json`, and `--timeout=1000` options.

**Config key vs config value**:
Use **config key** for the field name in a config file. Use **config value** for the assigned value and the selected setting it configures.

**Entry file vs input file**:
Use **entry file** for the initial script or module file path that starts a single program run. Use **input file** for files processed by batch-style tools or per-file result envelopes.

**Source type vs execution mode**:
Use **source type** for script-vs-module entry semantics. Use **execution mode** for interpreter-vs-bytecode execution.

**Native vs built-in**:
Use **native function** for Pascal-backed callables. Use **built-in** for APIs supplied by GocciaScript. Use **core language built-in** or **runtime global** when availability matters.

**Runtime surface vs runtime global**:
Use **runtime surface** for the aggregate host-exposed capability set. Use **runtime global** for one JS-visible global installed by a runtime extension.

**Function vocabulary**:
Use **user-defined function** for source-defined functions in general. Use **arrow function**, **ordinary function**, or **method** only when that semantic distinction matters.

**Script Loader**:
Use **Script Loader** for `GocciaScriptLoader`. Avoid **script executor** except when describing the generic act of executing source.

**Frontend/backend terminology**:
Avoid frontend/backend terminology in GocciaScript architecture. Use **source pipeline** for lexer/parser/AST work and **CLI host** for command-line programs.

## Example Dialogue

Dev: Should `fetch` be registered by the engine?
Domain expert: No. `fetch` is a runtime global installed by a runtime extension, usually through the loader runtime profile.

Dev: Is this an executor bug?
Domain expert: It depends. If interpreter mode and bytecode mode disagree, it is probably an executor issue. If both execution modes agree but the language behavior is wrong, it is an engine semantics issue.

Dev: Is `--mode=bytecode` a flag?
Domain expert: No. It is a CLI option because it takes a value. `--print` is a CLI flag because it is boolean.

Dev: Is `"mode"` a config option?
Domain expert: Call `"mode"` the config key. Call `"bytecode"` the config value.

Dev: Should this JSON envelope say entry files?
Domain expert: If it reports every file a batch run processed, say input files. Use entry file only for the initial program path.

Dev: Is `--source-type=module` module mode?
Domain expert: No. It sets the source type to module source. Execution mode is interpreter or bytecode.

Dev: The callback is a built-in function because it is written in Pascal, right?
Domain expert: It is a native function because it is Pascal-backed. It is only a built-in if the engine or runtime surface provides it as part of GocciaScript.

Dev: Can this test say "user function"?
Domain expert: Prefer "user-defined function" unless the test specifically means an arrow function, ordinary function, or method.
