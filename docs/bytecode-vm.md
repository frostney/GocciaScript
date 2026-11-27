# Bytecode VM

*For contributors working on the engine's compiler, register bytecode, interpreter and JIT.*

## Executive Summary

- **One engine** — every application in `source/app` compiles JavaScript to register bytecode (`Goccia.Compiler`, `Goccia.Instructions`) and runs it on the engine's VM (`Goccia.VM`); hosts drive it through `TGocciaEngine` (`Goccia.Embed`)
- **Register bytecode** — 64-bit instructions (8-bit opcode, 16-bit A and B, 24-bit C), one value stack per thread, a frame's registers holding `this`, parameters, locals and temporaries; captured variables are Lua-style upvalues
- **Three ways to run a function** — the interpreter (`Execute`, with `FastLoop` for the hot instructions), the baseline JIT (`Goccia.Jit.inc`) and the optimizing tier (`Goccia.Opt.inc`); the two compiled tiers exist only on Linux x86-64
- **Call bounds** — at most 200,000 nested calls below the entry's frame unless `--max-stack` sets fewer; native re-entry is bounded by the thread's native stack; proper tail calls reuse a frame past a depth floor ([ADR 0187](adr/0187-g2-reuses-a-frame-for-a-tail-call-only-past-a-depth-floor.md))
- **Binary format** — `.gbc` files (`Goccia.Artifact`) with `GBC\0` magic, format version 201, an instruction-set fingerprint and a verifier (`Goccia.ArtifactVerify.inc`); see [Precompiled bytecode modules](bytecode-loading.md)

## Overview

The engine keeps GocciaScript's front end (`Goccia.Lexer`, `Goccia.Parser`, the `Goccia.AST.*` units, `Goccia.SourcePipeline`) and replaces everything after the AST: compiler, bytecode, VM, object model, garbage collector and built-ins, all under `source/engine`. [docs/engine/README.md](engine/README.md) and [docs/engine/design.md](engine/design.md) describe the engine as a whole; this page covers the path from AST to executed instruction.

`Goccia.Compiler` walks the AST once per function and produces a `TFuncTemplate` (`Goccia.Instructions`) for each: code, constants, inline caches, upvalue descriptors, nested templates and per-instruction line and column tables. A closure is a function object pointing at its template and its upvalue array. Templates live as long as the engine that made them (`RegisterTemplate`, `FreeAllTemplates`).

Compilation is lazy by default (`Goccia.Lazy`, `GOCCIA_LAZY=0` turns it off). `Goccia.PreParse` checks a Script or Module's whole syntax and early errors and records, per function, its source range and the free names it uses. The parser then skips the bodies of eligible functions and the compiler makes a stub template for each; its first call compiles it (`LazyCompileHook`). The JavaScript preludes in `source/engine/prelude` are compiled at build time and embedded as a snapshot (`Goccia.Snapshot`); their nested bodies are read on first call too.

A Script's top level runs its global declarations through one `opDeclGlobals` instruction (ES2026 §16.1.7 GlobalDeclarationInstantiation). A module compiles to a generator over a table of binding cells (`opMakeModCell`, `opModGet`), linked and evaluated by `Goccia.Modules`. A direct `eval` in function code compiles its source as an arrow inside a stand-in for the caller, reaching the caller's bindings as upvalues; indirect `eval` and the `Function` constructor compile through `Goccia.Eval`.

## Pipeline

```text
Source -> JSX transform (optional) -> Goccia lexer/parser -> AST -> Goccia.Compiler -> TFuncTemplate -> Goccia.VM -> TValue
```

Bytecode files use the `.gbc` extension (`Goccia.Artifact`).

## Main Units

| Area | Units |
|------|-------|
| Instruction set, templates, inline cache records | `Goccia.Instructions` |
| AST to bytecode | `Goccia.Compiler` |
| Pre-parse and lazy compilation | `Goccia.PreParse`, `Goccia.Scan`, `Goccia.Lazy` |
| Run-time parsing (entry, `eval`, `Function`) | `Goccia.Eval` |
| Interpreter (`Execute`, `FastLoop`), frames, calls, limits | `Goccia.VM` |
| Baseline JIT | `Goccia.Jit.inc` (included in `Goccia.VM`) |
| Optimizing tier | `Goccia.Opt.inc` (included in `Goccia.Jit.inc`) |
| Values, cells, shapes, GC, language profile | `Goccia.Core` |
| Module records, linking, `.gbc` imports | `Goccia.Modules` |
| Bytecode files and their verifier | `Goccia.Artifact`, `Goccia.ArtifactVerify.inc` |
| Prelude snapshot | `Goccia.Snapshot` |
| Runtime profile (`--profile`) | `Goccia.Profile` |
| Embedding API | `Goccia.Embed` (`TGocciaEngine`) |

## Core Design

- **Values.** `TValue` is a NaN-boxed `QWord`: doubles as themselves, int32, the special constants (`undefined`, `null`, `false`, `true`, the hole) and heap cell pointers under tags. Numbers never allocate ([design.md](engine/design.md#values)).
- **One value stack per thread.** `VStack` holds `STACK_MAX` (4 Mi) values, reserved as zeroed pages. A frame's registers are a window of it: `R0` is `this`, then the parameters, locals and temporaries. `TFrame` records the closure, template, base, PC, return slot, argument count and kind. An engine's state is its thread's: `VStack`, `Frames` and the handler stack are threadvars.
- **Calls.** `opCall A, B, C` calls `R[A]` with `this` in `R[A+1]` and `B` arguments from `R[A+2]`; bit 0 of C (instruction bit 40) means `this` is `undefined` and bit 1 (bit 41) marks a tail call. A call between bytecode functions pushes a `TFrame` and continues in the same `Execute` activation, so JavaScript recursion does not recurse on the native stack. A native that calls back into JavaScript (`CallValue`, `Construct`, `GeneratorResume`) nests `Execute(StopFrame, Res)` on the native stack.
- **Upvalues.** A captured local stays in its register while its frame lives; an open upvalue points at its stack index. `CloseUpvals` copies it into its cell when the scope or frame ends (`opClose`, a return, an unwinding throw).
- **Exceptions.** `opTry` pushes a `THandler` (frame, handler PC, exception register, kind: catch or finally); `opEndTry` pops it. A throw sets `Pending` and jumps to the unwind code in `Execute`, which drops frames down to the handler's. The dispatch loop has no Pascal `try`.
- **Generators and async functions.** `opGenStart`, `opYield`, `opAwait`, `opYieldStar` and `opYieldStarAsync` suspend a frame by saving its registers; the compiler records how many are live at each suspension so that only those are kept.
- **Garbage collection.** The VM shares `Goccia.Core`'s heap with the built-ins. The value stack up to `StackTop` is a root, and the nursery collector moves young cells, so code re-reads a cell pointer from a root after anything that can allocate. See [Garbage Collector](garbage-collector.md) and [design.md](engine/design.md#heap-and-gc).
- **Strict types.** With strict-types on (`ProfileStrictTypes`), the compiler emits `opCheckType` before a store into an annotated binding or one whose initializer has an evident type, and for an annotated parameter. Return-type annotations are not checked. See [Type Annotations](type-annotations.md).

## Instruction Format

`Goccia.Instructions` encodes each instruction as a `QWord`:

| Bits | Field | Read with |
|------|-------|-----------|
| 0–7 | opcode (`TOp`) | `InsOp` |
| 8–23 | A (unsigned 16) | `InsA` |
| 24–39 | B (unsigned 16) | `InsB` |
| 40–63 | C (signed 24, or unsigned) | `InsC`, `InsCU` |

`Ins(Op, A, B, C)` builds one. Jumps carry a signed offset in C relative to the next instruction. Constants, inline caches, upvalues and child templates are indices into the template's tables: `opLoadK` reads `Consts[C]`, `opGetProp A, B, C` reads `R[B]`'s property `ICs[C].Key` into `R[A]`.

## Opcode Layout

`TOp` declares 169 opcodes, grouped by purpose rather than by hotness:

- loads and moves, upvalues, globals and global lexical bindings (`opGetGLex`, `opSetGLex`)
- property and element access through inline caches, `delete`, `in`, `instanceof`, `super` references
- generic arithmetic, bitwise and comparison operators, plus immediate forms (`opAddI`, `opSubI`, `opIncI`, `opBitAndI` through `opUShrI`)
- jumps, including compare-and-jump forms (`opJNLt`, `opJNLe`, `opJNGt`, `opJNGe`, `opJNSEq`, `opJSEq`) and nullish/undefined tests
- calls, construction, `super()`, returns, spread calls (`opCallV`, `opNewV`)
- object and array literals, class setup, fields, private names and private members
- iteration (`opGetIter`, `opIterNext`, `opForIn`, the async iterator steps), `yield`, `yield*`, `await`
- module cells, `import()`, `import.meta`, `with` object environments, direct-eval hints
- `opIntrinsic` (calls into the `builtins5` prelude for constructs the compiler lowers), `opCheckType`, and the coverage counters `opCovStmt`, `opCovBranch` and `opCovFn`

The hot set is a table, not a range: `InitFastOps` marks the 79 opcodes `FastLoop` has fast paths for (`FastOp`). Opcode numbers are the enum's order, so adding, removing or renaming an opcode changes `EngineFingerprint` and every `.gbc` file written before it is refused.

## Execution Tiers

### Execute

`Execute(StopFrame, Res)` in `Goccia.VM` is the full interpreter: one `case` over every opcode, run from `Frames[FrameTop - 1]` until the frame `StopFrame` returns. It reloads its dispatch state at `LReload` after a call or return switches frames and unwinds at `LThrow`. Execute owns everything the faster paths hand back: allocation-heavy instructions, anything that can throw, realm switches, coverage (`CovStep`), the runtime profile and instruction counting.

### FastLoop

`FastLoop` holds the fast paths of the hot instructions in a function small enough for FPC to keep its loop variables in machine registers. Before an instruction marked in `FastOp`, `Execute` calls it; it runs until an instruction it does not handle, or whose fast path does not apply, and returns that instruction's PC without having started it. It also runs calls of plain bytecode functions and their returns, so a call does not end the fast path; it then returns -1 and `Execute` reloads from the frame record. It never returns from `StopFrame`. A store the write barrier would have to remember, and any instruction of a bytecode file that trusts a register's kind, are left to `Execute`.

`Execute` skips `FastLoop` while an instruction limit, coverage or the runtime profile is armed, so every instruction is counted or recorded.

### Baseline JIT

On Linux x86-64 (`GOCCIA_JIT` in `Goccia.Checks.inc`), `FastLoop` enters a template's machine code through `XJitRun`. A template is compiled after `JIT_HOT` (20) entries (`GOCCIA_JIT_HOT` overrides; `GOCCIA_JIT=0` turns the JIT off) by `JitCompile`, which emits straight-line code for each instruction's fast path. The frame stays the interpreter's: registers live in the value stack and no machine register holds a value across instructions, so the code can stop before any instruction. A failed guard jumps to that instruction's exit, which returns its PC to `FastLoop` or `Execute`.

Compiled code calls a compiled callee through a shared call stub (`JitBuildCallStub`, `XJitCall`), one native activation per call. `JitTooDeep` bounds that nesting by the native stack (`JIT_STACK_MARGIN` above the thread's limit, or `JIT_MAX_DEPTH` activations where the bounds are unknown); past it the interpreter runs the frames. Async functions are not compiled; a generator's body runs compiled between its suspensions. Code is written to bump-allocated executable chunks through `JitPublish`, which serializes every core before the code runs ([ADR 0186](adr/0186-g2-serializes-every-core-before-new-jit-code-runs.md)). Only the thread that holds the JIT (`JitTakeThread`) compiles; the applications give it to one file thread at a time.

Nothing is compiled while a timeout, an instruction budget, a `--max-stack` bound, coverage or the runtime profile is in force (`JitBlocked`): compiled code polls only the thread's interrupt flag at back edges and checks the fixed frame bound.

### Optimizing Tier

`Goccia.Opt.inc` compiles a region of a baseline-compiled template (everything reachable from its entry or from a loop header) to typed SSA: numbers and booleans become values in general-purpose and XMM registers, typed by a fixpoint seeded with the frame's values at compile time; tagged values stay in their stack registers. A template's counter runs down at its entry and back edges, and a region is optimized after `OPT_HOT` (1,000) of them (`OptMaybe`, `OptCompile`). Small callees are inlined (at most `OPT_INLINE_MAX` instructions each, four levels deep). An inlined call pushes no `TFrame`; while an embedded instruction of an inlined callee runs, the frame record's inline descriptor names the inlined calls (`InlinedFrameCount`), which `Function.prototype.caller` reads.

An instruction the tier does not take ends its block with a side exit: live numbers are boxed back into the stack and the baseline code continues there. A failed guard deoptimizes the same way and records feedback per instruction (`OptFb`), so the next compilation speculates less; after `OPT_MAX_DEOPTS` the regions are dropped, and after repeated recompiles the template stays baseline. Among other conditions, templates with a `try`, an `arguments` object, a rest parameter, a top level, async code, or kind-trusting instructions from a bytecode file are not eligible. `GOCCIA_OPT=0` turns the tier off and `GOCCIA_OPTLOG=1` logs each region.

## Frames and Call Bounds

`FRAME_MAX` (200,003) is the frame record count a thread holds: 200,000 nested calls below the entry's frame plus three a run keeps. `SetMaxFrames(N)` lowers the bound to `N` nested calls; 0 or anything larger keeps 200,000. Hosts set it through `TGocciaEngine.MaxFrames`, which GocciaRunner, GocciaTestRunner, GocciaBenchmarkRunner and GocciaScriptLoaderBare fill from `--max-stack=N`. Without the option the bound is 200,000. A test function's frames count from its own frame (`CallRebased`).

Every frame push checks `FramesFull`; past the bound the call throws `RangeError: Maximum call stack size exceeded` (`XFrameOverflow`). When the frames' stack memory plus the live heap is past the memory limit, the error carries the stack-memory suggestion. A memory refusal delivered at a call with at least 1,024 frames on the stack is reported as the same stack overflow ([ADR 0185](adr/0185-g2-refuses-memory-at-the-next-safe-point.md)). A spent timeout drops the frame bound to 0, so the next call aborts without an extra check per call.

Native re-entry has its own bound. `Goccia.Core` keeps `NATIVE_STACK_MARGIN` (512 KB) above the thread's stack limit; the native entries into the interpreter refuse with the same `RangeError` once the stack pointer is below that floor (`ReentryTooDeep`), and a frame push from native code has a backstop 256 KB below it. A getter, a `toString`, a sort comparator or a generator's `next()` called from a built-in therefore fails cleanly whatever the frame bound. A spread call whose arguments would pass the end of the value stack throws the same error.

See [Execution limits](engine/README.md#execution-limits) for timeouts, instruction budgets and the memory limit.

## Proper Tail Calls

`Goccia.Compiler` sets bit 41 of an `opCall` or `opCallV` in tail position (ES2026 §15.10.1 IsInTailPosition, §15.10.2 HasCallInTailPosition): the value of a `return` or a concise arrow body, through `?:`, `&&`, `||`, `??` and a comma expression's last operand, in strict code that is not a generator or async function, with no try block, `using` declaration or `for-of` loop still to act after the call (`TailEligible`, `CollectTailCalls`). A catch block without a finally, and a finally block, are tail positions.

The VM acts on the bit only once the frames are deep: past 1,024 (`TAIL_FLOOR`), or half a smaller `--max-stack` bound. There `Execute` replaces the calling frame with the callee's (`XTailCall`: the frame's upvalues closed, its handlers dropped, its `this` and arguments moved to its base, its return slot and kind kept), which is what PrepareForTailCall (§15.10.3) allows. `FastLoop` hands such a call to `Execute`, and the baseline JIT exits to the interpreter at it. Below the floor a tail call is an ordinary call in every tier, so stack traces keep their frames and compiled code its native calls and inlining. A construction's frame, a callee that is not a bytecode function, a generator or async callee, and another realm's function keep their frames (`XTailCallOk`). A chain of tail calls therefore holds at most about 1,024 frames however long it runs ([ADR 0187](adr/0187-g2-reuses-a-frame-for-a-tail-call-only-past-a-depth-floor.md), [Engine README](engine/README.md#proper-tail-calls)).

## Performance Direction

The engine's speed comes from the representation and from moving work out of the general interpreter:

- NaN-boxed values, so arithmetic on numbers allocates nothing and type tests are tag compares
- immediate operands for `x + k`, `x - k`, `++`/`--` and bitwise operators with a small integer constant, and compare-and-jump instructions for conditions, so a loop test is one instruction
- `TFuncTemplate.FastCall` (not async, no rest parameter, no `arguments` object): such a callee's frame is pushed directly, clearing only the registers past its arguments (`FillUndef`)
- `NoCaptures` and `NoHandlers`, computed by `FinishTemplate`, let a return skip closing upvalues and popping handlers
- inline caches on every property, global and method access (below), and constructor caches on the template (`CtorFnShape`, `CtorProtoSlot`, `FIShape`) so `new F()` finds the prototype and field initializer without lookups
- lazy compilation and the prelude snapshot, which keep start-up cost proportional to the code that runs
- native fast paths for prelude-backed APIs (RegExp, `String.prototype.replace`/`split`/`match`) while the objects involved are unmodified ([design.md](engine/design.md#built-ins))

Measured comparisons with QuickJS are in [docs/engine/README.md](engine/README.md#status) and [Benchmarks](benchmarks.md).

### Dispatch layout

FPC spills whole live ranges, the variable with the most interferences first. In `Execute`, which spans the whole instruction set, the dispatch variables (PC, the register base, the code pointer) therefore live in memory, and PC's store-to-load chain bounds the dispatch rate. `FastLoop` exists for that reason: it calls little, allocates only where an instruction must, and keeps its loop variables in registers. It reads operands from the instruction in memory through an instruction pointer rather than from a decoded word. Keep it that way:

- add a fast path to `FastLoop` only for an instruction that is hot across workloads, and put its slow path in a `noinline` helper (`XFastGetElem`, `XFastNative`)
- keep managed types (strings, dynamic arrays) and `try` blocks out of `Execute`, `FastLoop` and their hot helpers: FPC gives such a function an exception frame and initialize/finalize calls on every call; check the disassembly for `fpc_pushexceptaddr`
- read a threadvar in a statement of its own in hot code; the units carry notes where FPC 3.3.1 miscompiled a combined access

### Inline Caches

Each `TPropIC` (`Goccia.Instructions`) belongs to one instruction:

- **Property gets** check the receiver's shape against `Poly0` inline. A second shape allocates `More`, for up to four entries in all. An entry records the receiver's shape and object kind, the prototype depth of the holder (up to `IC_MAX_DEPTH`, 8), each prototype's expected shape, and for dictionary-mode prototypes their version. An accessor entry (`Acc`) lets `FastLoop` call a bytecode getter inline.
- **Megamorphic sites**, whose four entries are full, look the receiver shape and key up in a shared 4,096-entry table before walking the chain.
- **Stores** record the receiver shape and slot, or for an adding store the transition's target (`NewShape`). A prototype change clears every cached add transition (`ForgetAddTransitions`).
- **Globals** cache the global object's shape and slot; a global lexical binding is cached by its index.

Shapes are outside the collected heap. A major collection retires the shapes of prototypes nothing reaches, and `ForgetRetiredShapes` (with the JIT's and the megamorphic table's counterparts) resets every cache that names one before a retired record can be reused ([design.md](engine/design.md#heap-and-gc)).

## Profiling

`--profile` reports through GocciaScript's profilers (`Goccia.Profiler`, `Goccia.Profiler.Report`), fed by `Goccia.Profile`; see [Profiling](profiling.md). Under the runtime profile every instruction runs through `Execute`'s counted path (no `FastLoop`, no compiled code), so the step hook sees each opcode, each consecutive pair, and whether a binary arithmetic or comparison instruction had two Number operands. Opcodes carry the engine's names (`GocciaOpcodeName`). Functions are timed from frame pushes; the preludes are not profiled and their cost is their caller's. Because profiling changes the tier code runs in, compare timings only between runs with the same profile flags.

## Runtime Error Diagnostics

A runtime fault names what the author wrote:

- **Call sites.** Each template's `CallSites` table maps a call or construct instruction to the callee's source text. When the callee is not callable or not a constructor, `XThrowNotFunction` and `XThrowNotCtor` use it, so `obj.missingMethod()` reports `obj.missingMethod is not a function` with a suggestion. The table is written to `.gbc` files, so loaded code reports the same message.
- **Binding names.** A temporal-dead-zone check (`opCheckHole`) names its binding from a constant, as does a const assignment (`Assignment to constant variable 'x'`). A module binding read too early is named through `ModSlotNameHook`.
- **Positions.** Every instruction has a line and column (`Lines`, `Cols`), also written to `.gbc` files.
- **Provenance.** `AttachStack` gives an engine-made error its `stack` when the language profile wants stack traces (`ProfileStackTraces`; the conformance host records only where the error was made). `ErrorStackProvenance` returns the stack as the engine wrote it, whatever the script did to the property since, and `TGocciaEngine` locates an uncaught error only from that, never from a thrown value's own `stack` string. The CLI renders a code frame only for the entry or a file the run loaded as a module (`TGocciaEngine.ModuleLoaded`), never for a path a thrown value names or a module the host evaluated first.

See [Errors](errors.md) for the messages and stack format.

## Instruction Limit

`LimitsArm(TimeoutMs, MaxInstructions)` arms a run's limits. With an instruction budget, `Execute` decrements `InstrLeft` for every instruction and skips `FastLoop`, and built-ins whose loops run as long as the script says poll the same counter (`PollInterrupt`), so the count is exact and the same on every run. A spent budget aborts with `Execution exceeded instruction limit of N`; no `catch` or `finally` runs. The JIT stays off under a budget. See [Execution limits](engine/README.md#execution-limits).

## Binary Format

`Goccia.Artifact` writes and reads `.gbc` files:

- Magic: `GBC\0`
- Format version: `GOCCIA_GBC_VERSION` (201; older files must be recompiled)
- Runtime tag: `goccia-engine`
- Engine fingerprint: a hash of the opcode names and the intrinsic count (`EngineFingerprint`), so a build with another instruction set refuses the file
- Little-endian; strings are UTF-16 code units after a u32 length

The header continues with the source path and the compile profile (`TGocciaCompileProfile`'s fields, then the engine's unrestricted flag) and the kind (Script or Module). The body holds the source texts the templates' `Function.prototype.toString` ranges point into, the module record (`TModuleInfo`) or a Script's import text, and the top-level template with every nested function compiled eagerly. Constants carry explicit kinds (double, int32, special, string, BigInt decimal text), never raw value bits that could forge a pointer or a hole. Inline caches are stored as their keys. Code for a file names no global lexical binding by its index in the compiling engine's table (`SetArtifactCompile`); such bindings are found by name at run time.

Binary loading is a validation boundary. The reader bounds every length and count against what is left, limits files to 64 MiB (`GOCCIA_GBC_MAX_FILE_BYTES`) and nesting to 256 functions (`GOCCIA_GBC_MAX_DEPTH`), rejects unknown opcodes, flags and constant kinds, and rejects trailing bytes. The verifier (`VerifyTemplateShape`, `VerifyCode`) then checks every operand of every instruction against the template's registers, constants, caches, upvalues and children; jump targets; a terminator at the end; the same exception handler depth on every path to an instruction; the global-declaration and generator prologues; and direct-eval caller descriptions. It refuses instructions the compiler never emits and rewrites every suspension to keep all its registers. What the code cannot show (a register holding the array, object, closure, private name or module cell table an instruction trusts it to hold) is checked at run time for templates read from a file (`TFuncTemplate.Loaded`, `Trusting`): `Execute` checks it, `FastLoop` and the baseline JIT hand those instructions to `Execute`, and the optimizing tier leaves such templates alone. Class field definitions (`opDefField`) are checked in every tier.

`GOCCIA_ARTIFACT_ROUNDTRIP=1` writes, reads and verifies every compiled Script and module before it runs; `GOCCIA_ARTIFACT_SCRAMBLE=<seed>` corrupts register operands after reading to test the run-time checks. [docs/engine/README.md § Bytecode files](engine/README.md#bytecode-files) lists the test scripts.

## Design Rationale

The engine was built to answer whether an FPC engine designed for speed can match QuickJS while passing test262 ([Engine README](engine/README.md)). The VM is specific to GocciaScript: its instructions encode JavaScript semantics directly, and it shares one object model and heap with the built-ins.

### Why Register-Based?

Register bytecode needs fewer instructions per operation than a stack machine: an instruction names its operands, so `a + b` on two locals is one `opAdd` with no pushes or pops, and locals that are never captured never leave their registers. The compiler follows Lua's model: locals in registers, captured locals reached through upvalues that close when their scope ends.

### Why FastLoop beside Execute?

FPC's register allocator gives the dispatch variables of a function as large as `Execute` no machine registers. Splitting the hot instructions into `FastLoop` keeps a small loop that FPC compiles well, while `Execute` stays the single place that implements every instruction completely.

### Why Three Tiers?

The interpreter runs everything and is the fallback for every exit. The baseline JIT removes dispatch and operand decoding for hot templates without changing the frame layout, so it can stop at any instruction. The optimizing tier removes boxing and redundant checks in hot regions and inlines small callees, and relies on the baseline code for every exit and deoptimization.

### Compiler-Side Desugaring

The compiler lowers syntax to existing instructions where a dedicated opcode would add nothing:

- `?.`, `??` and `??=` compile to `opJNullish` and `opJNotNullish` jumps
- `&&`, `||` and `!` in a condition become chains of jumps on each operand (`BranchTrue`, `BranchFalse`), except under coverage, which keeps each operator's branch records
- `for-of` uses `opGetIter`, `opIterNext` and `opIterClose`; `for-in` uses `opForIn`, whose key iterator steps without a call
- `using` and `await using` call the disposal helpers (`Goccia.Disposal`) held under internal keys on the global object
- decorators, enums and pattern matching call helpers of the `builtins5` prelude through `opIntrinsic` (`INTRINSIC_*` in `Goccia.VM`)

### Compatibility Scope Helpers

The language profile (`TLangProfile` in `Goccia.Core`) decides which sloppy-mode forms compile. An unrestricted profile, used by the conformance host and the preludes, has them all; a restricted one has only those its compatibility flags name ([Language](language.md)).

- **Non-strict code** (`--compat-non-strict-mode`). Without it every Script and function is strict. With it, a non-strict function's `undefined` or `null` `this` becomes the global object at call time (ES2026 §10.2.1.2 OrdinaryCallBindThis). Assignment to an unresolvable name goes through `opSetGlobal`, which creates a global property in sloppy code and throws a `ReferenceError` in strict code; `delete` of a global name is `opDelGlobal`.
- **`with`** (§14.11). The object is converted once (`opWithObj`) into a hidden local; each identifier the body may resolve through it is tested with `opWithHas` (honouring `@@unscopables`) before `opWithGet` or `opWithSet`, falling back to the ordinary binding.
- **`arguments`** (`--compat-arguments-object`). A function that reads `arguments` gets an object in a register (`ArgsReg`). It is mapped (§10.4.4.7 CreateMappedArgumentsObject) when the function is non-strict with simple parameters and the compiler cannot rule out aliasing (`MappedArgs`); otherwise it is unmapped (§10.4.4.6). Arrows and eval code use the enclosing function's.
- **`var`** (`--compat-var`). A function's `var` bindings are registers; a Script's top-level `var` and function declarations become global object properties through `opDeclGlobals` and `opDefGlobalFunc`.

### Compiler Optimizer

`Goccia.Compiler` does no constant folding and no separate optimization pass. Its choices are local to the expression being compiled:

- `x + k` and `x - k` with a small integer literal become `opAddI` or `opSubI` (`x - 0` stays a subtraction, to keep `-0 - 0` as `-0`); bitwise and shift operators with an int32 literal take the immediate forms
- a relational or strict-equality test in a condition becomes one compare-and-jump instruction
- an operand already in a register is read in place, and copied first only when a later operand may change it (`OperandMayChange`)
- a lexical binding gets a temporal-dead-zone check only where a read can precede its declaration

Type speculation, unboxing and inlining belong to the optimizing tier, which works from the values it observes rather than from static proofs.

### Compiler Type Inference

The compiler does not infer types to select instructions: arithmetic always compiles to the generic opcodes, and the optimizing tier specializes them at run time. The only static typing is strict-types: `StrictTypeCode` maps an annotation, or without one an initializer's evident type (a literal, template, object or array literal, function or construction), to an `opCheckType` code through `Goccia.Types.LocalType`, and every later store into that binding is checked ([Type Annotations](type-annotations.md)).

### How Opcode Additions Work

Add an opcode only when an operation is common and its semantics are settled; prefer lowering syntax to existing instructions, or a flag in an operand when it is a mode of an existing instruction. A new opcode needs:

- its `Execute` case, and a `FastLoop` case only if it is hot
- a decision in `Goccia.ArtifactVerify.inc`: operand checks, or refusal in files; if it trusts a register's kind, membership in `KIND_TRUSTING_OPS` and a run-time check for loaded templates
- the baseline JIT's handling in `JitInstr0`, or nothing (the code then exits to the interpreter at it), and the optimizing tier's in `OInstr`, or a side exit
- regenerated bytecode: the new enum changes `EngineFingerprint`, so existing `.gbc` files are refused; the prelude snapshot is checked only against the prelude sources, so build through `build.pas`, which compiles the preludes again and embeds the new snapshot

## Related documents

- [Engine README](engine/README.md) — status, build, limits, applications, gates
- [Engine design](engine/design.md) — values, heap, objects, execution and compilation in brief
- [Precompiled bytecode modules](bytecode-loading.md) — `.gbc` files from the bundler and runner
- [Architecture](architecture.md) — source pipeline and engine layers
- [Garbage Collector](garbage-collector.md) — the heap the VM shares
- [Core patterns](core-patterns.md) — recurring Pascal conventions
- [GocciaScript Context](../CONTEXT.md) — canonical project terminology

## Contributor Notes

- A `TValue` held in a Pascal local is stale after any allocation: root it (`PushRoot`, `RootAt`) or re-read it from a rooted place. `scripts/engine/gcstress.sh` finds the misses.
- `Execute` uses `goto` for its reload and unwind paths and has no Pascal `try`; keep JavaScript exceptions on the handler stack and `Pending`, never Pascal exceptions, inside the dispatch loop.
- A throw that a native entry cannot catch leaves through `Execute(StopFrame, ...)` returning `False` with `Pending` set; natives return `False` the same way.
- Keep the operand meanings in `Goccia.ArtifactVerify.inc` in step with `Execute`: the compiled tiers check nothing themselves, so the verifier and `Execute`'s run-time checks are the bound for code read from a file.
- The engine's Pascal is outside `./format.pas`; follow the surrounding style.
- Run the engine gate for VM changes ([Engine README § Gate](engine/README.md#gate)), with GC stress for anything near allocation or rooting, and the artifact round-trip and tamper tests for instruction or verifier changes.
