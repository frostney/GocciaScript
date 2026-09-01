# Bytecode VM

*For contributors working on the bytecode executor and VM.*

## Executive Summary

- **Two execution modes** — tree-walk interpreter (default) and bytecode VM (`--mode=bytecode`), sharing the same source pipeline, runtime objects, and GC
- **Executor abstraction** — `TGocciaBytecodeExecutor` implements `TGocciaExecutor` and drives only the compiler and VM; two residual couplings remain — direct `eval`, and a module's top-level function declarations, which are created and run by the tree-walk evaluator
- **Goccia-owned VM** — executes directly on `TGocciaValue` with tagged `TGocciaRegister` values; not a generic VM layer
- **Opcode space** — core instructions (0-127) for hot paths, non-core generic ops (128-166), and semantic/helper instructions (167-255) for colder operations like imports/exports
- **Binary format** — `.gbc` files with little-endian encoding, `GBC\0` magic, and version constant

## Overview

GocciaScript has two execution modes:

- **Interpreter mode**: tree-walk execution over the AST via `TGocciaInterpreterExecutor`
- **Bytecode mode**: AST compilation to Goccia bytecode, then execution on `TGocciaVM` via `TGocciaBytecodeExecutor`

Both execution modes are implementations of `TGocciaExecutor` (see [Architecture](architecture.md#executor-architecture)). The single `TGocciaEngine` class bootstraps the core language environment (global scope, core built-ins, shims) and delegates execution to whichever executor is configured. Optional runtime globals are attached through runtime extensions. The `TGocciaBytecodeExecutor` unit itself depends only on the compiler and VM; the VM it drives, however, still calls the tree-walk evaluator for direct `eval` (`TGocciaVM.ExecuteDirectEval` → `EvaluateEvalProgram`), so the bytecode path is not yet fully independent of the evaluator.

An imported module adds a second such coupling. Its environment is initialized while linking (ES2026 §16.2.1.7.3.1 InitializeEnvironment), and the module loader creates the top-level function declarations there with the tree-walk evaluator (`HoistFunctionDeclarations`). Bytecode compilation of that module therefore reuses those preinitialized bindings for exported declarations instead of compiling them (`PreinitializedTopLevelFunctions`), and their bodies keep running under the evaluator in bytecode mode. Everything an evaluator path can be handed from such a body — including a compiled `TGocciaVMClassValue` reached by `new`, `super()`, or a bound wrapper — must therefore work in both directions; `TGocciaClassValue.UsesOwnInstantiation` and `TryConstructOnReceiver` are what route construction of a compiled class back to the VM. `scripts/differential/l-modulefndecl.test.js` gates this split.

## Pipeline

```text
Source -> JSX Transformer (optional) -> Lexer -> Parser -> Compiler -> Goccia Bytecode -> TGocciaVM -> TGocciaValue
```

Public bytecode artifacts use the `.gbc` extension.

## Main Units

| Area | Units |
|------|-------|
| Opcode definitions | `Goccia.Bytecode.pas` |
| Function templates / constants | `Goccia.Bytecode.Chunk.pas` |
| Module format | `Goccia.Bytecode.Module.pas` |
| Binary I/O | `Goccia.Bytecode.Binary.pas` |
| Debug metadata | `Goccia.Bytecode.Debug.pas` |
| VM execution | `Goccia.VM.pas` |
| Frames / closures / upvalues | `Goccia.VM.CallFrame.pas`, `Goccia.VM.Closure.pas`, `Goccia.VM.Upvalue.pas` |
| Bytecode executor | `Goccia.Executor.Bytecode.pas` (`TGocciaBytecodeExecutor`) |
| Numeric proof / lowering | `Goccia.Compiler.NumericProof.pas`, `Goccia.Compiler.Expressions.pas` |
| Opcode name lookup | `Goccia.Bytecode.OpCodeNames.pas` |
| Profiler | `Goccia.Profiler.pas`, `Goccia.Profiler.Report.pas` |

## Core Design

- The register file uses tagged values that keep scalars unboxed until they cross a runtime boundary (see [Design Direction](architecture.md#design-direction)).
- The VM uses the same value classes as the interpreter: arrays, objects, classes, promises, functions, symbols, enums, and built-ins.
- `undefined`, `null`, booleans, and hole values use shared singleton objects.
- Sparse arrays use `TGocciaHoleValue.HoleValue`, not raw `nil`.
- The VM is integrated with the shared garbage collector and shared call stack.
- Call stack depth is tracked per frame (`FFrameDepth`) and enforced against a configurable limit (default 2 900 frames, `--stack-size=N`). Exceeding the limit throws a `RangeError: Maximum call stack size exceeded`. Pass `--stack-size=0` to disable the limit. Bytecode-to-bytecode calls use a trampoline (`FFrameStack`) so the Pascal call stack stays flat regardless of JS call depth.
- Bytecode mode enables strict type enforcement through compiler-emitted checks and typed opcodes.

## Opcode Layout

The opcode space is split into three tiers:

- `0..127`: core VM instructions
- `128..166`: non-core generic arithmetic/bitwise operations
- `167..255`: semantic helper/orchestration operations

In the current VM:

- core instructions cover hot execution paths such as locals, arithmetic, comparisons, property/index access, calls, construction, iteration, and class/object setup
- semantic instructions already include generic arithmetic and bitwise operations in `128..140`
- module and async orchestration currently starts at `167` (`IMPORT`, `EXPORT`, `AWAIT`, `IMPORT_META`)

The current encoding helpers are defined in `Goccia.Bytecode.pas`:

- `EncodeABC`
- `EncodeABx`
- `EncodeAsBx`
- `EncodeAx`
- `DecodeOp`
- `DecodeA`
- `DecodeB`
- `DecodeC`
- `DecodeBx`
- `DecodesBx`
- `DecodeAx`

Current instruction families:

- load and move: constants, literals, locals, upvalues
- control flow: jumps, handlers, throw, return
- closures: function templates and captured upvalues
- typed arithmetic and comparison
- object and array operations
- class construction and member definition
- calls, construction, iteration, globals, and string coercion
- opt-in compatibility scope helpers: unmapped `arguments` object creation and `with` object binding probes
- semantic-only imports/exports, dynamic import, import.meta, await, and yield

Some opcode families intentionally use flags or mode operands instead of one opcode per syntax form. For example:

- accessor definition uses constant-key and dynamic-key instructions plus getter/setter and static/instance flags
- collection helpers use a shared opcode (`OP_COLLECTION_OP`) for object spread, object rest, and iterable-to-array spread
- validation uses a shared opcode for require-object and require-iterable checks
- generator metadata is serialized and a single `OP_YIELD` drives generator suspension; `yield*` uses the same opcode with delegation bookkeeping rather than a second opcode
- bytecode generators suspend by snapshotting the VM continuation at `OP_YIELD` (instruction pointer, live registers, local cells, handler state, and GC-visible references) and resume from that snapshot instead of replaying from function entry

Current opcode design rules:

- add explicit Goccia opcodes for stable, hot, language-owned behaviour
- do not introduce generic VM naming into new instructions
- do not add an opcode for something already reachable through existing call dispatch — if a built-in (e.g. `Object.freeze`) already goes through `OP_CALL_METHOD`, emit that call sequence from the compiler rather than adding a new opcode or sub-mode
- prefer mode operands on shared opcodes over proliferating single-purpose opcodes for cold or infrequent operations

## Performance Direction

Recent VM cleanup and optimization work has focused on reducing per-instruction overhead without reintroducing old abstraction layers:

- cache and reuse shared primitive values directly in registers
- avoid eager allocation of closure cells for uncaptured locals
- pre-size argument collections for calls and construction
- hold call arguments in a stack-disciplined arena window (`FArgumentStack` with a base+count window, mirroring the register and local-cell stacks) instead of a per-call dynamic array, so an ordinary call performs no argument-array allocation; frame save/restore and native re-entry store `(base, count)` rather than copying
- defer stack-trace frames on the hot call path: push the function-template pointer rather than copying its name/source strings, and materialise them only when a trace is captured (see [ADR 0074](adr/0074-deferred-bytecode-call-stack-frames.md))
- execute compiler-proven closed-world numeric self-calls through `OP_CALL_SELF_NUM`: recursive calls with one to three scalar arguments use a compact register frame while sharing the generic entry frame's closure, lexical environment, local-cell and argument windows, realm, and execution context (see [ADR 0101](adr/0101-closed-numeric-scalar-self-call-frames.md))
- use unchecked template access in the dispatch loop where bounds are already guaranteed
- fuse `Number - Int16` as `OP_SUB_NUM_IMM` and conditional `Number <= Int16` as `OP_JUMP_IF_NUM_NOT_LTE_IMM` only when the compiler proves the source is an ECMAScript Number; these instructions remove literal-load and branch dispatches rather than merely replacing a generic arithmetic dispatch
- fuse `local.ident` as `OP_GET_LOCAL_PROP_CONST`: one instruction that reads the local slot (including the `OP_GET_LOCAL` TDZ hole check) and then the existing `OP_GET_PROP_CONST` shape-lite IC; computed keys, optional chaining, `with` lookups, import bindings, and global-backed identifiers keep the unfused path
- retain a static named import's linked module namespace in its local/upvalue slot: `OP_IMPORT` scales with declarations, while repeated identifier reads use `OP_GET_IMPORT_BINDING` to dereference the cached live binding identity without repeating module-loader lookup
- read standalone `this` properties directly from a non-captured local register, preserving the derived-constructor guard while avoiding a temporary-register move; captured, top-level, and method-call receiver paths retain their existing lowering
- keep fast register access limited to proven hot/simple paths; local-slot and complex property paths should only move to fast access when they stay correct and measurably improve throughput
- the register, local-cell, and argument window fills are GC-safety/correctness critical (the GC marks the whole live window): they are deliberately retained rather than trimmed

### Inline Caches

Four per-site inline caches live on `TGocciaFunctionTemplate`, all indexed by the instruction's name-constant index, all runtime-only (never serialised to `.gbc`):

- **Global reads** (`OP_GET_GLOBAL`) — `TGocciaGlobalReadCacheEntry` validates `(scope identity, binding-map entry version)` and re-reads the binding by entry index, skipping the name hash.
- **Own property reads** (`OP_GET_PROP_CONST`, `OP_GET_LOCAL_PROP_CONST`) — `TGocciaPropertyReadCacheEntry` validates the receiver's interned **shape** (`Goccia.Values.Shape`): same shape implies the same key at the cached entry index, so one site hits across many same-layout receivers. The descriptor kind is re-checked on every hit because data-to-accessor redefinition keeps the entry index.
- **Prototype-resolved reads** (`OP_GET_PROP_CONST`, `OP_GET_LOCAL_PROP_CONST`, after an own miss) — `TGocciaProtoReadCacheEntry` proves continued *absence* of the name on the receiver and intermediate levels and *presence* at the holder, all by fresh shape identity per level, then re-reads the holder descriptor by entry index. The live chain is re-walked per hit, so `setPrototypeOf` is followed inherently; chain levels must be exact `TGocciaObjectValue`; chains deeper than two levels and accessor holders stay generic. Class instance methods (data properties on the class prototype object) are the dominant beneficiary.
- **Own property writes** (`OP_SET_PROP_CONST`) — `TGocciaPropertyWriteCacheEntry` is the write-side counterpart of the own-read cache: same `(shape, entry index)` validation, storing only own writable data properties. `Writable` and the descriptor kind are re-checked on every hit. Accessors, proxies, private fields, deletion, prototype mutation, non-writable descriptors, and non-ordinary receivers take `AssignProperty`. This cache is independent of the read/proto slot map so the read-side PIC is not expanded ([ADR 0088](adr/0088-reject-broader-property-inline-caches.md)).

Hits and fills serve only exact-class `TGocciaObjectValue` / `TGocciaVMLiteralObjectValue` / `TGocciaInstanceValue` receivers, so overridden lookup semantics (proxies, exotic objects, private names) always take the generic path. Shapes are computed lazily at fill time (`EnsureShape`), not eagerly at property-append time: a stale shape is a true prefix description of an append-only layout, so the hit path may read it raw and at worst misses. Delete/clear flip a map to dictionary mode (a sentinel shape that never matches a cache entry). A map also flips to dictionary mode when `EnsureShape` runs from a non-owner realm, so cross-realm property reads never intern one realm's layout into another realm's shape table. After `PROPERTY_READ_CACHE_POLYMORPHIC_LIMIT` (reads) or `PROPERTY_WRITE_CACHE_POLYMORPHIC_LIMIT` (writes) consecutive misses-with-refill or fill declines a site is megamorphic: it stops probing and serves gated receivers through the uncached own-data fast path.

Cached pointers (scope, shape) are compared for identity only and never dereferenced. Scope cache entries carry an entry-version stamp against allocator address reuse; shape entries need none, because shapes are never freed within an engine's lifetime, function templates never outlive their engine, and cross-realm maps stop shape tracking before a foreign realm can cache their owner layout.

Computed property access (`OP_ARRAY_GET`/`OP_ARRAY_SET`, `OP_GET_INDEX`/`OP_SET_INDEX`, `OP_DEL_INDEX`) shares one key-classification and receiver-dispatch implementation (`ClassifyPropertyKey` plus the `ExecGet/ExecSet/ExecDeleteComputedProperty` cores in `Goccia.VM.pas`); per-opcode semantic differences are explicit `TGocciaComputedAccessOptions`, not divergent copies. A non-BigInt `TGocciaTypedArrayValue` receiver at an array-index key takes an unboxed element fast path (`TryReadIndexedScalar`/`TryWriteIndexedScalar`): reads move the element straight into a register scalar and numeric-scalar writes store it directly, so neither allocates the heap `TGocciaNumberLiteralValue` or index-name string the generic object branch would. Mutable fixed-length `ArrayBuffer` views additionally retain their constructor-proven data block and length, checking only detachment and the element index on each access; resizable, shared, and immutable backing buffers retain the general synchronization path. BigInt kinds, non-index keys, and non-scalar write values fall through to the boxed path; an out-of-range or detached **read** does too (yielding `undefined`). A non-BigInt scalar **write**, however, keeps its integer-indexed exotic semantics in place even for an out-of-range index or immutable backing buffer — the store is skipped and reported as successful, never boxed. All value semantics are preserved, including the observable `ToNumber` ordering of integer-indexed `[[Set]]`.

The current optimization target is reducing bytecode-mode suite time further without diverging interpreter and bytecode semantics.

Numeric call-derived proof is deliberately closed-world. It applies only to a
simple, const-bound local arrow with no escape, optional call, default/rest/
destructured parameter, mixed argument, or unsupported use. Every external
direct call must supply exactly one numeric literal per parameter, and the
expression body must establish a Number result recursively. The proof marks
parameters for the two numeric immediate superinstructions; for one to three
parameters it also emits `OP_CALL_SELF_NUM` at non-tail direct self-call sites.
The scalar frame performs no speculative type conversion or deoptimization:
unsupported shapes retain ordinary `OP_CALL`, and tail calls retain the generic
proper-tail-call path. The optimization does not turn the function into a
generally typed function or change generic `+` semantics.

## Profiling

The `--profile` option on GocciaScriptLoader enables language-level profiling of the bytecode VM. See [profiling.md](profiling.md) for the full guide.

- `--profile=opcodes` — opcode frequency histogram, opcode pair frequency (superinstruction candidates), and scalar fast-path hit rate for generic arithmetic/comparison opcodes
- `--profile=functions` — per-function self-time, total-time, call count, and heap allocation count
- `--profile=all` — both
- `--profile-output=path.json` — JSON export

The profiler follows the same singleton-tracker pattern as coverage (`Goccia.Coverage.pas`). When profiling is disabled, a predictable boolean guard remains in the dispatch loop. Enabled-mode overhead depends on the workload and profiling mode, so measure it on the corpus being investigated rather than relying on a fixed percentage.

## Runtime Error Diagnostics

A runtime fault must read identically in both execution modes. Three pieces of
machinery keep that true:

- **Call-site descriptors.** `TGocciaFunctionTemplate` carries a runtime-only
  table mapping a call/construct instruction's start PC to the callee as the
  author wrote it (a `TGocciaCalleeDescriptor` from `Goccia.Error.CallDiagnostics`)
  plus the call expression's own line and column. The compiler fills it while
  emitting `OP_CALL`, `OP_CALL_METHOD`, `OP_CONSTRUCT`, `OP_CONSTRUCT_SPREAD`
  and tagged-template calls — a **compile-time cost paid once per call site** (a
  descriptor copy plus a whitespace-normalising pass over the callee's source
  text), not a per-instruction runtime cost. The VM reads the table only when
  the callee turns out not to be callable or constructable, so
  `obj.missingMethod()` reports `obj.missingMethod is not a function` rather
  than `undefined is not a function`. The evaluator derives the same descriptor
  straight from the AST, and both modes format the message and suggestion
  through the same functions. The table is **not** serialised to `.gbc`: a
  module loaded from binary bytecode falls back to the runtime-type-name form of
  the message (see the note below).
- **Throw-path source positions.** Deferred call frames carry no position
  ([ADR 0074](adr/0074-deferred-bytecode-call-stack-frames.md)), which left
  every bytecode-mode stack frame at `file:0:0` and the runner with no line to
  render a code frame for. `TGocciaCallStack.SetTopFrameLocation` stamps the
  executing frame with a position. The dispatch loop's ordinary
  call/property/index instructions never call it, so their throughput is
  unchanged; it runs on the failure paths (a fault about to be raised) and, in
  addition, once per `new` reaching `ConstructValue` — a native constructor such
  as `new Error(...)` captures its trace *during* construction, before any
  throw, so its caller frame must be stamped up front (a small per-`new` cost: a
  binary search over the function's call-site table plus two string
  assignments). `TGocciaVM.StampThrowLocation` recovers the current instruction
  for throw paths outside the dispatch loop through a pointer probe into the
  innermost loop's `Template`/`InstructionStartIP` locals, saved and restored
  once per native re-entry.
- **Frame source is provenance-bound, not `stack`-selected.** A code frame is
  rendered only from provenance the engine records on a genuine error *when it
  is created* — the top call frame's source location, plus a ±context excerpt of
  that module captured from the engine's own per-scope source store. Both are
  carried on the error object (`TGocciaErrorObjectValue`, an error-only subclass
  so the fields never enlarge the base object's GC-charged size). The thrown
  value's `stack` string is guest-writable and is never parsed to choose a file,
  a line, or an excerpt: a forged `throw { stack: "…at f (/etc/passwd:2:1)" }`
  object is not engine-created, carries no provenance, and renders no code frame
  at all — so no host ever opens a guest-named path and the sandbox's
  `sandbox.fs.path` gate cannot be bypassed through diagnostics. See
  `Goccia.Values.ErrorHelper.AttachErrorSourceProvenance`,
  `Goccia.Diagnostics.SourceRegistry`, and docs/module-resolution.md
  "Runtime code frames".
- **Principal exclusion — no host source to a guest.** Ownership is decided at
  load, not inferred at capture. Host enrollment (`--globals`,
  `--host-environment`, `--module`, `--modules`, manifests/configs, and embedding
  injection) stamps the root module host-owned. Static, dynamic, and deferred
  imports inherit that ownership transitively from the importing module, even
  when the load begins later from a host-exported function called by the guest.
  A host-injected virtual module is host-owned; imports reached only from a guest
  module are guest-owned. Entries use file identity from the handle that read
  the content: POSIX device+inode or Windows volume-serial+file-index. Symlinks,
  junctions, hardlinks, and case aliases therefore cannot mint a guest-owned
  second copy. If identity lookup fails, there is no lexical-path fallback and
  source disclosure for the scope fails closed. An excerpt is captured only
  from guest-owned source, so a genuine error thrown inside a host module —
  including a transitive import, a `--module` the guest imported, or a held
  `Error` the host created — shows its location but no source excerpt.
- **Executing-engine scope, enforced again at render.** Each engine's module
  loader owns one scope, identified by a durable process-monotonic principal;
  registration targets the loader's own scope, and capture targets the scope the
  engine activates around its execution (`RunModuleForSourceType` for bytecode,
  `Execute`/`ExecuteProgram` for the interpreter) — and around every cross-engine
  transition (`ActivateRealmExecutionContext` for a ShadowRealm `evaluate`/
  `importValue`/wrapped function activates the child scope and restores it on
  return). The excerpt is additionally stamped with its principal and re-checked
  when rendered: each host explicitly passes its expected principal to
  `Goccia.Error.Detail`, which renders source only when the two values match.
  Zero/no supplied principal means location-only; no active execution scope is
  never treated as authorization. A child's error formatted by a resumed parent
  is therefore refused the child's source even though the object crossed the
  boundary. Because the excerpt is captured while its scope is live, a host may
  still render it after `Engine.Free` only if it retained and supplies the
  originating engine's principal.
- **Byte-accurate diagnostic memory.** Retained module source is accounted as
  its actual UTF-16 line representation, including entry/list storage, pointer
  slots, and separately allocated line strings. A captured excerpt is charged
  as its retained UTF-16 string allocation. The identical byte figure is used
  for caps, `--max-memory` reservation, and release. Reservation refusal yields
  location-only, and transactional registry insertion rolls back partial map
  keys and the reservation if allocation fails during commit.
- **Construct/native-call frame stamping.** A successful `OP_CONSTRUCT` /
  `OP_CONSTRUCT_SPREAD` snapshots and restores the caller frame around
  `ConstructValue`, so a constructor's position does not leak onto a later throw
  (`new Map(); JSON.parse("{")` reports the JSON fault at its own line, not the
  `new`). Native calls stamp the call site around the invoke, likewise restored,
  so a native callee's error carries a location rather than `0:0`.

### `.gbc` parity note

The call-site descriptor table is intentionally runtime-only, so a module
executed straight from a compiled `.gbc` (rather than compiled in-process from
source) has no descriptors: its non-callable/non-constructor faults fall back to
the runtime-type-name form (`undefined is not a function`) and carry no
suggestion, whereas an in-process compile would name the callee. This is the one
diagnostic-parity gap left open. Serialising the table would close it at the
cost of a new bytecode-format section (callee text, object/property text, kind,
line, column per call site) and its verifier; that is a bounded but real format
change, deferred rather than taken here because the `.gbc` path is not on the
default runner or test surface. Revisit if binary modules become a primary
execution path.

## Instruction Limit

The dispatch loop supports an optional instruction counter (`Goccia.InstructionLimit.pas`). When armed, the counter increments on every dispatched instruction and the limit is checked at the top of each iteration. When disabled, only the guard read of the limit threadvar remains on the hot path. See [Embedding — Execution Limits](embedding.md#execution-limits) for the full API and interpreter-mode behavior.

## Binary Format

- Magic: `GBC\0`
- Version constant: `GOCCIA_FORMAT_VERSION`
- Endianness: little-endian
- File extension: `.gbc`

Binary loading is a validation boundary. The reader rejects oversized files,
declared strings or tables that exceed the remaining stream, excessive nested
function templates, invalid enum tags, and trailing data. Before a module can
execute, the verifier checks opcodes, register destinations, constant and
function references, control-flow targets, and exception-handler metadata.
The VM retains bounds checks on instruction, constant, and function access as
defense in depth.

## Current Status

- `--mode=bytecode` runs the Goccia VM directly.
- The full JavaScript suite passes in bytecode mode.
- The old generic VM/runtime bridge has been removed from the active build.

## Design Rationale

GocciaScript includes a bytecode executor built specifically for GocciaScript. The current VM is not a language-agnostic subsystem: it executes directly on `TGocciaValue`, shares the same runtime objects as the interpreter, and uses a Goccia-owned opcode surface.

### Why a Bytecode VM?

The tree-walk interpreter directly evaluates AST nodes via recursive function calls. This is simple and debuggable, but carries overhead from VMT dispatch on every AST node, deep call stacks for nested expressions, and no opportunity for instruction-level optimization. A bytecode VM trades compilation cost for faster execution: flat instruction dispatch, register-based operands, and a compact in-memory representation.

### Why Register-Based?

Stack-based VMs (like the JVM and WASM) are simpler to compile to and have smaller instruction encoding. Register-based VMs (like Lua 5, LuaJIT, and Dalvik) need fewer instructions per operation and avoid redundant stack manipulations. Register-based was chosen for execution performance.

### Why Three Tiers?

The solution is a split opcode space with three tiers:

- **Core range (0–127):** register, control-flow, closure, literal, and other hot/stable VM operations.
- **Non-core generic range (128–166):** generic arithmetic and bitwise operations that are still explicit bytecode but handle mixed or untyped operands.
- **Semantic helper range (167–255):** colder language-level orchestration operations such as imports/exports, dynamic import, `import.meta`, await, and resource disposal.

This split keeps the dispatch surface organized while still allowing the bytecode executor to be explicitly Goccia-specific.

### Why Shared Runtime Values?

The VM shares the `TGocciaValue` object model with the interpreter rather than maintaining a second value representation. Registers use `TGocciaRegister` — a tagged variant record (`Goccia.VM.Registers.pas`) that keeps booleans, integers, and floats unboxed as scalars. Values only cross into `TGocciaValue` when they leave the register file (e.g., property access, function calls, GC marking).

That choice removes:

- conversion layers between interpreter values and VM values
- duplicate object models for arrays, objects, classes, and promises
- bridge-only GC root management
- bytecode/runtime disagreement over `undefined`, `null`, and sparse array holes

The trade-off is that arithmetic fast paths are split between scalar register operations (typed opcodes like `OP_ADD_INT` / `OP_ADD_FLOAT`) and generic `TGocciaValue` fallbacks (like `OP_ADD`).

### Compiler-Side Desugaring

Language features are compiled into compact bytecode instruction sequences rather than expanding the opcode surface unnecessarily:

- **Nullish coalescing (`??`) and nullish coalescing assignment (`??=`)** — The compiler emits `OP_JUMP_IF_NOT_NULLISH` in its nullish-match mode, so `undefined`, `null`, and internal hole values all follow the same short-circuit path without extra comparison instructions.
- **Template literals** — The compiler parses interpolations at compile time, emits string constants and `OP_TO_STRING` for expression parts, then chains `OP_CONCAT` instructions.
- **Object literals** — Data properties compile to `OP_DEFINE_DATA_PROP` so object initializers create or overwrite own enumerable data properties. Concise methods use `OP_DEFINE_METHOD_PROP` to attach `[[HomeObject]]` without changing plain data-property function or arrow values. Ordinary property assignment still uses the `OP_SET_*` family and keeps `[[Set]]` prototype-chain semantics.
- **Object spread** — The compiler emits dedicated Goccia bytecode rather than routing through a generic extension dispatcher.
- **Increment/decrement (`++`/`--`)** — The compiler emits fused numeric update opcodes for increment/decrement sites. Prefix or discarded-result sites use `OP_INC_NUMERIC`/`OP_DEC_NUMERIC`; postfix sites with distinct result and storage registers use `OP_POST_INC_NUMERIC`/`OP_POST_DEC_NUMERIC` so the old numeric value is produced while the binding/property value is updated. All variants preserve BigInt, convert other inputs through `ToNumeric`, and keep the read/convert/write side effects required by ES2026 §13.4.4.1.
- **Traditional `for` lexical bindings** — `let`/`const` loop initializers keep the full per-iteration environment path whenever closures, direct eval, suspension, destructuring, pattern matching, `with`, `using`, or nested declaration boundaries can observe binding identity. Plain generated/counting loops share the loop lexical scope and avoid the otherwise redundant copy-in/copy-out sequence.

This keeps the emitted bytecode compact and makes opcode additions deliberate instead of reactive.

### Compatibility Scope Helpers

Compatibility features that alter identifier lookup still compile to explicit VM state instead of falling back to interpreter behavior.

- **`arguments` object** — With `--compat-arguments-object` enabled, function templates snapshot the current call arguments in the frame. `--compat-non-strict-mode` does not enable this helper by itself. `OP_CREATE_ARGUMENTS` materializes the object into the declared local slot before parameter defaults and body execution, so default initializers can observe `arguments.length` and generators see the original call list after suspension/resume. Operand `B` selects mapped semantics for sloppy simple parameter lists and operand `C` carries the formal parameter count; the VM forces those parameter locals into cells so indexed properties alias parameter bindings even if the object escapes. Strict functions, modules, and non-simple parameter lists use unmapped arguments objects.
- **Non-strict `this` binding** — Function templates serialize their strict-this mode. With `--compat-non-strict-mode` enabled for script source, ordinary function templates clear it so VM call paths coerce nullish `this` to `globalThis`; arrows and class methods keep their existing lexical or strict receiver behavior. Module source ignores the compatibility flag for this decision.
- **Non-strict assignment** — Failed object/global writes throw by default. In script source non-strict compatibility mode, the compiler emits `OP_SET_PROP_CONST_LOOSE`, `OP_SET_INDEX_LOOSE`, and `OP_SET_GLOBAL_LOOSE` for ordinary writes so failed `[[Set]]` results are ignored while null/undefined property access and throwing setters still raise errors.
- **`with` statement** — With `--compat-non-strict-mode` enabled for script source, the compiler lowers `with (expr) body` to `OP_TO_OBJECT`, stores the object in a hidden local, and records that hidden binding in the compiler scope. Identifier reads, writes, updates, and identifier calls inside the dynamic extent emit `OP_HAS_WITH_BINDING` probes from innermost to outermost hidden object before falling back to normal local/upvalue/global resolution. Writes that resolve to a with object use the loose set opcodes in non-strict mode. Nested functions inherit the hidden binding as an upvalue when captured, preserving closures created inside `with`.
- **Non-strict `delete`** — With `--compat-non-strict-mode` enabled for script source, member deletes emit `OP_DELETE_PROP_CONST_LOOSE` or `OP_DEL_INDEX_LOOSE`, which preserve strict null/undefined errors but return `false` for non-configurable properties. Identifier deletes compile to local/upvalue false results, `OP_DELETE_GLOBAL` for global object property semantics, or `with` binding probes as needed.

### Compiler Optimizer

Bytecode compilation includes a small compile-time value optimizer. It folds pure primitive constant expressions, propagates immutable local `const` bindings initialized from compile-time constants, and omits branches or statement tails that are provably unreachable.

The optimizer is intentionally compiler-side only:

- it does not add opcodes or change the `.gbc` format
- it does not track mutable bindings, imports, destructuring, function/class declarations, or global-backed top-level bindings
- it only uses `--strict-types` for conservative algebraic simplifications where the strict type alone preserves JavaScript semantics

When coverage is enabled, `PreserveCoverageShape` keeps constant branch structure in the emitted bytecode so coverage can report the non-hit branch instead of erasing it from the report.

### How Opcode Additions Work

New opcodes should be added only when an operation is both common enough and semantically stable enough to justify a dedicated instruction.

Prefer:

- explicit Goccia opcodes for core language/runtime behaviour
- compiler lowering to existing instructions for syntactic sugar
- flags or operands when an operation is a mode of an existing instruction rather than a new concept

### Tier 1 Property Flags vs Tier 2 Visibility

Property **mutability** (writable/configurable) is still a VM concern. Bulk operations like freeze and seal remain derived from the lower-level property-flag operations:

- `SetEntryFlags(key, flags)` — modify flags on a single property
- `PutWithFlags(key, value, flags)` — create a property with specific flags
- `PreventExtensions` — stop new properties from being added
- `Freeze` = iterate all entries, set flags to 0, prevent extensions (a convenience, not a primitive)

Property **visibility** and **accessor semantics** remain part of the higher-level object/class model rather than low-level property-flag storage.

### Spread Calling Consolidation

Spread-based calls use the flags byte on `OP_CALL` and `OP_CALL_METHOD`. Spread is treated as a mode of the call instruction rather than as a separate opcode family.

### Rejected Findings

During code review, the following findings were investigated and determined to be non-issues:

- **`SBIAS_24` (`Goccia.Bytecode.pas`)** — The 24-bit signed bias constant 8388607 is correct. The 24-bit unsigned range 0..16777215 centered at 8388607 gives a signed range of −8388607..+8388608. This is standard Lua-style bias encoding.

## Related documents

- [Architecture](architecture.md) — Shared source pipeline and both execution modes at a glance
- [Interpreter](interpreter.md) — Tree-walk execution (`Goccia.Interpreter`, `Goccia.Evaluator.*`)
- [Core patterns](core-patterns.md) — Recurring Pascal conventions
- [GocciaScript Context](../CONTEXT.md) — Canonical project terminology

## Contributor Notes

- Do not add new bytecode/runtime concepts under old generic naming.
- Prefer `TGoccia*` bytecode and VM types in new code.
- Keep interpreter and bytecode semantics aligned through shared runtime objects, not conversion layers.
