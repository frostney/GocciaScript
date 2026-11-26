# Core patterns

*Recurring implementation conventions in the Pascal codebase (values and rooting, natives, built-in registration, realms, the parser) and implementation detail for shared vocabulary such as Define vs Assign.*

## Executive Summary

- **Immediate special values** — `undefined`, `null`, `true`, `false` and the array hole are constants of the NaN-boxed `TValue` (`VUndefined`, `VNull`, ...), and Int32 and double Numbers are stored in the value itself: none of them allocates
- **Rooting** — the GC moves young cells, so a `TValue` held in a Pascal local is stale after any allocation; natives keep values on the temp root stack (`PushRoot`, `RootAt`, `PopRoots`) and register threadvars that hold values with `AddRoot`
- **Native convention** — a native is a `TNativeFn`: `Args[0]` is `this`, `Args[1..Argc]` the arguments; it writes `Result` and returns `False` with the exception in `Pending`
- **Builder pattern** — each built-in unit has an Init procedure that makes its prototypes and constructors with `NewPlainObject`, `DefineNative`, `DefineOwn` and friends, and registers its per-realm threadvars with `RealmVals`
- **Define vs Assign** — `Define` creates a property or binding (`DefineOwn`, `opDefField`, `opMakeGLex`); `Assign` changes an existing one through `[[Set]]` or the binding's checks (`SetV`, `opSetProp`, `opSetGLex`) — distinct operations throughout the codebase

For **canonical terminology**, see [GocciaScript Context](../CONTEXT.md). For **pipelines and layers**, see [Architecture](architecture.md). For **execution**, see [Bytecode VM](bytecode-vm.md). For the engine's own design notes and working rules, see [Engine](engine/README.md) and [Engine design](engine/design.md).

## Recurring implementation patterns

These patterns show up across layers (values, heap, natives, built-ins, parser). They are implementation conventions, not separate subsystems.

### Rooting values across allocation

The engine's nursery is a copying collector: a minor collection moves every live young cell, so any allocation can move what a Pascal local points at. A `TValue` held in a local is therefore stale after anything that may allocate (and every helper that can run JavaScript may allocate). Natives keep such values on the thread's temp root stack and read them back from it:

```pascal
{ Goccia.Builtins }
procedure DefineGetterData(O: TValue; const Name: UnicodeString; Fn: TNativeFn; Data: UInt32);
begin
  PushRoot(O);
  PushRoot(NewNativeFunctionData(Fn, 'get ' + Name, 0, Data));
  PushRoot(MkPtr(NewAccessor(RootAt(TempTop - 1), VUndefined)));
  DefineOwn(RootAt(TempTop - 3), AtomOf(Name), RootAt(TempTop - 1), pfConfigurable or pfAccessor);
  PopRoots(3);
end;
```

- `PushRoot`, `RootAt` and `PopRoots` (`Goccia.Core`) are inline and work on `TempRoots`, a stack of up to 4 Mi values whose pages are committed as they are touched. Pop exactly what you pushed on every exit path.
- A threadvar that holds a `TValue` for longer than one call (a prototype, a cached function) is registered once with `AddRoot` (`Goccia.VM`); `AddRootRange` registers several. Host units that register on first use guard it with `FirstInEngine`, so a thread's next engine registers again.
- Atoms (interned property keys), shapes and function templates live outside the collected heap and do not move.
- Storing a young value into an old cell goes through the write barrier (`WriteBarrier`, `ValueArrayStore`, `SetSlot`), which remembers the old cell for the next minor collection.

`scripts/engine/gcstress.sh` and `scripts/engine/gcstress-tests.py` run the bundles and `tests/` with a tiny nursery and `--gc-verify`, which finds a missed root as a crash or a wrong value ([Engine § Gate](engine/README.md#gate)).

### Native functions

Every native is a `TNativeFn` (`Goccia.Core`):

```pascal
TNativeFn = function(Args: PValue; Argc: Integer; var Result: TValue): Boolean;
```

`Args[0]` is `this` and `Args[1..Argc]` are the arguments. Each built-in unit declares a small inline `Arg(Args, Argc, I)` that returns `VUndefined` past `Argc`, which is the ECMAScript rule for missing arguments (ES2026 §18: a built-in given fewer arguments than it requires behaves as if the rest were `undefined`). A native that throws sets `Pending` and returns `False`; the helpers it calls (`CallValue`, `Construct`, `GetV`, `SetV`, `ToStringV`, `ToNumberV`, ...) follow the same convention, so a failed step returns at once:

```pascal
{ Goccia.Atomics }
function Atomics_Pause(Args: TArgs; Argc: Integer; var Res: TValue): Boolean;
var V: TValue;
begin
  V := Arg(Args, Argc, 1);
  if (V <> VUndefined) and (not IsNumber(V) or IsNaN(NumOf(V)) or IsInfinite(NumOf(V)) or (Frac(NumOf(V)) <> 0)) then
    Exit(ThrowTypeError('Atomics.pause argument must be an integral Number'));
  ReadWriteBarrier;
  Res := VUndefined;
  Result := True;
end;
```

`ThrowError(Kind, Msg)` and `ThrowTypeError` (`Goccia.VM`) make the error in the running realm, set `Pending` and return `False`, so `Exit(ThrowTypeError(...))` is the idiom. There is no arity validator: a built-in checks what its algorithm checks, in the order the specification takes the steps. A native that loops as often as the script says (an array-like's `length` up to 2^53 - 1) polls the run's limits each step with `PollInterrupt`. Per-function data rides on the function object: `NewNativeFunctionData` stores a small integer the native reads from `NativeData` (the Set methods that share Map's natives pass 1).

### Builder pattern (built-in registration)

Each built-in unit has an Init procedure, which `InitIntrinsics` (`Goccia.Engine`) runs in a fixed order when an engine starts and again for each new realm. It registers its roots and per-realm state, then builds the objects with a few helpers: `NewPlainObject(Proto, InlineCap)`, `DefineNative(O, Name, Fn, Length)`, `DefineOwn(O, Key, V, Flags)`, `DefineHidden`, and in `Goccia.Builtins` `MakeCtor`, which makes a native constructor, links it to its prototype and binds it on the global object. Abbreviated from `InitBuiltins`:

```pascal
C := MakeCtor(Set_Ctor, 'Set', 0, SetProtoObj);
N(SetProtoObj, 'add', SetProto_Add, 1);
NS(SetProtoObj, 'has', MapProto_Has, 1);     { a Map native, with data 1: its receiver is a Set }
NS(SetProtoObj, 'values', MapProto_Keys, 0);
{ 24.2.4.11 / 24.2.4.18: keys and @@iterator are the values function }
GetV(SetProtoObj, AtomOf('values'), G);
DefineOwn(SetProtoObj, AtomOf('keys'), G, pfHidden);
```

- `pfHidden` (writable and configurable, not enumerable) is the attribute set ES2026 §18 gives built-in data properties unless a clause says otherwise; a constructor's `prototype` property is defined with no flags.
- When two standard properties must hold the **same function object** (`Set.prototype.keys === Set.prototype.values`, `Map.prototype[Symbol.iterator] === Map.prototype.entries`), read the first back and define it under the second name, as above. Do not register a second native.
- `DefineOwn` with the `prototype` key on a native while intrinsics are initializing marks that native as a constructor.
- A built-in whose specification steps matter more than its speed is written in a [JavaScript prelude](#javascript-preludes) instead.

### Ordered own property keys

`OwnKeys(O, OnlyEnumerable)` (`Goccia.VM`) returns an object's string keys in ES2026 §10.1.11.1 OrdinaryOwnPropertyKeys order: array indices ascending (the elements, a String object's characters, a typed array's elements and index keys kept in the shape), then the other strings in creation order. `OwnPropertyKeysV` (`Goccia.ObjModel`) is the full `[[OwnPropertyKeys]]`, symbols last, and a proxy's `ownKeys` trap result goes through `ProxyOwnKeys` with its invariant checks. `for-in` (`ForInKeys`) and object spread (`CopyDataProps`) consume these lists without sorting them again.

### Binary expression levels (parser)

Each left-associative precedence level in `Goccia.Parser.pas` is its own short loop: parse the next-tighter level, then fold operators of this level into a `TGocciaBinaryExpression` (through `MakeBinary`) while they appear:

```pascal
function TGocciaParser.Addition: TGocciaExpression;
var
  Op: TGocciaToken;
begin
  Result := Multiplication;
  while PeekWithLexicalGoal(glgInputElementDiv).TokenType in [gttPlus, gttMinus] do
  begin
    Op := Advance;
    Result := MakeBinary(Result, Op.TokenType,
      Multiplication, Op.Span);
    CountChainLink;
  end;
end;
```

Levels that need extra checks keep them inline. `Equality`, for example, accepts `gttEqual`/`gttNotEqual` and only builds a node for `gttLooseEqual`/`gttLooseNotEqual` when loose equality is enabled (`--compat-loose-equality`); otherwise it records an unsupported-feature warning.

### Shared prototypes

Every instance of a built-in type has the same `[[Prototype]]`, as ECMA-262 lays it out: all `Set` instances share the one `Set.prototype` of their realm. In the engine that is structural. The prototype is an ordinary object held in a per-realm threadvar (`SetProtoObj`, `ArrayProto`, `PromiseProto`, ...), and an object's shape names its prototype: `NewPlainObject(Proto, ...)` starts the object at `RootShapeFor(Proto)`, so every object made from one prototype starts from one root shape, and the inline caches that recorded it hit for all of them.

A native constructor called with `new.target` takes its instance's prototype from it, as ES2026 §10.1.14 GetPrototypeFromConstructor requires, through `ProtoFromNewTarget(Default, P)`: when `new.target.prototype` is not an object, the default becomes the same intrinsic of the constructor's realm (`GetPrototypeFromConstructor`, `MapIntrinsic`), not of the running realm. A prelude class marks itself with `markIntrinsic` (`MarkIntrinsicClass`) to get the same fallback.

Natives on a prototype use `Args[0]` for the instance; for chaining (`Set.prototype.add`, `Map.prototype.set`) they return `Args[0]`.

### Realm Ownership & Slot Registration

A realm's intrinsics live in threadvars while the realm runs (ES2026 §9.3). Each Init procedure registers the threadvars that hold its realm's state, and `SwitchRealm` (`Goccia.VM`) saves those values for the running realm and loads another's:

| API | Registers | Use |
|-----|-----------|-----|
| `RealmVals([@X, ...])`, `RealmValRange` | `TValue` threadvars | Prototypes, constructors, cached intrinsic functions |
| `RealmPtrs([@X, ...])` | pointer threadvars | Per-realm shapes and other pointers |
| `RealmInts([@X, ...])` | integer threadvars | Per-realm counters and indices |

`CreateRealm` (ES2026 §9.3.1 InitializeHostDefinedRealm) clears every registered threadvar and the global lexical table, sets `RealmInit` and runs the Init procedures again, then the host's part (`RealmHostInit`). While `RealmInit` is set, registration calls are ignored and `AddRoot` does not register a root twice, so the procedures run unchanged; state that every realm of an engine shares (the well-known symbols, a job list) is made under `if not RealmInit then`. From `Goccia.Atomics`:

```pascal
procedure InitAtomics;
begin
  AddRoot(@SharedBufferProto); AddRoot(@AtomicsObj); AddRoot(@Scratch); AddRoot(@AsyncPromises); AddRoot(@SABCtor);
  RealmVals([@SharedBufferProto, @AtomicsObj, @SABCtor]);
  if not RealmInit then
  begin
    { the agent's Atomics.waitAsync waiters }
    AsyncPromises := MkPtr(NewArray(0, 8));
    AsyncCount := 0; AsyncPending := 0; AsyncDeadlines := nil; AsyncLive := nil;
  end;
  SharedBufferProto := MkPtr(NewPlainObject(ObjectProto, 8));
  // ... the constructor, its methods, and the Atomics namespace object ...
end;
```

A function object carries its realm's index (`FnRealm`); a call to another realm's function switches to that realm and back when it returns. `GetFunctionRealm` is ES2026 §7.3.24.

Engines never share realms: each engine builds its first realm in `EngineInit`, and `EngineDone` frees everything it made, so mutations of intrinsics, including non-configurable property additions, do not reach the thread's next engine. See [ADR 0032](adr/0032-per-engine-realm-isolation.md) for the rationale and [Architecture § Realm Isolation](architecture.md#realm-isolation) for the layout.

#### Unregistered realm state (do not do this)

A threadvar that holds realm state but was not registered keeps the last realm's value after a switch: code of one realm then sees another realm's intrinsic. Register it with `RealmVals`, `RealmPtrs` or `RealmInts` in the Init procedure that sets it. On Linux x86-64, `GOCCIA_REALMCHECK=<size of the TLS segment>` makes `CreateRealm` report every thread-local byte that the new realm's Init procedures changed and the switch back did not restore, which finds such state. Do not cache realm-scoped objects in process-wide `var`s either: other engines run on other threads.

## Terminology

The canonical project glossary lives in [GocciaScript Context](../CONTEXT.md). This section expands implementation details for the Define vs Assign distinction.

### Define vs Assign

This distinction is critical in the codebase. Defining creates a property or binding and runs no setter; assigning changes an existing one and runs the checks the language attaches to it.

| Target | Define | Assign |
|--------|--------|--------|
| Object property | `DefineOwn` (native set-up: an own property with the given flags), `DefinePropertyOrThrow` (`Goccia.ObjModel`, `[[DefineOwnProperty]]` with validation), `opDefProp` and `opDefField` (object literals and class fields: CreateDataPropertyOrThrow) | `SetV`, `SetVS`, `SetVT`, `SetElemV` and `opSetProp` / `opSetElem`: `[[Set]]`, which walks the prototype chain, runs setters and proxy traps, and in strict code throws where `[[Set]]` returns false |
| Global lexical binding | `opMakeGLex` (a Script's `let`, `const`, `class`), `InjectGlobalBinding` (a host's `--global`), `BindGlobalImport` (an explicit Script's imports) | `opSetGLex` (`GLexSet`): a `TypeError` for a `const`, a `ReferenceError` in the temporal dead zone |
| Global `var` and function | `opDeclGlobals`, `opDefGlobalFunc` (ES2026 §16.1.7 GlobalDeclarationInstantiation) | `opSetGlobal` |
| Module binding | `opMakeModCell` (an export's cell) | An import binding is immutable |

## Design Rationale

### NaN-boxed Values and Kind Dispatch

A value is a `TValue`, a NaN-boxed `QWord` ([Engine design](engine/design.md)): a double is stored as itself below `$FFF9 0000 0000 0000` (NaN canonicalized), and above it the top 16 bits tag an Int32 (`$FFF9`), a special value (`$FFFA`: `undefined`, `null`, `false`, `true`, the hole) or a heap cell pointer (`$FFFB`). `MkNumber` stores an integral Number in Int32 range, other than `-0`, as an Int32. The inline helpers of `Goccia.Core` test and build values: `IsInt`, `IsDouble`, `IsNumber`, `IsString`, `IsObject`, `IsCallable`, `ObjOf`, `StrOf`, `MkInt`, `MkDouble`, `MkNumber`, `MkPtr`.

Every heap thing is a cell with an 8-byte header (`TCellHeader`: size, kind, flags). The cell kind tells strings, symbols, BigInts, value arrays, upvalues, accessors and objects apart, and an object's kind (`TObj.OKind`: `okPlain`, `okArray`, `okFunction`, `okNative`, `okBound`, `okProxy`, `okPromise`, `okTypedArray`, ...) selects its exotic behaviour. Code dispatches with a `case` on the kind, or a set test as `IsCallable` does:

```pascal
function IsCallable(V: TValue): Boolean; inline;
var O: PObj;
begin
  Result := False;
  if not IsObject(V) then Exit;
  O := ObjOf(V);
  Result := (O^.OKind in [okFunction, okNative, okBound]) or ((O^.OKind = okProxy) and ((O^.OFlags and 16) <> 0));
end;
```

Behaviour that lives in a later unit (a proxy's `[[Set]]`, `import()`, `ToObject`) is reached through a procedure variable that unit sets during its Init procedure (`ProxySetHook`, `ImportHook`, `ToObjectHook` in `Goccia.VM`), which keeps the unit graph acyclic without virtual methods.

**Why NaN-boxing and kind dispatch?**

- **No allocation for Numbers or special values** — arithmetic, comparisons and loop counters never touch the heap.
- **One word per value** — a value fits a machine register, the value stack is an array of `QWord`s, and the baseline JIT and the optimizing tier test tags and unbox numbers in a few instructions.
- **Plain records** — cells are records the collector can copy and scan by kind; there is no class instance, VMT pointer or constructor frame per value.
- **Cheap identity** — strict equality of non-numbers and special values is a `QWord` compare.

`TGocciaError` and its subclasses are Pascal exceptions of the front end; no JavaScript value is a Pascal object.

### Centralized Keyword Constants

The front end's JavaScript keyword literals are named constants in two units — `Goccia.Keywords.Reserved.pas` for reserved keywords and `Goccia.Keywords.Contextual.pas` for contextual keywords:

```pascal
// Goccia.Keywords.Reserved.pas
const
  KEYWORD_THIS       = 'this';
  KEYWORD_SUPER      = 'super';
  KEYWORD_NULL       = 'null';
  // ... more reserved keywords

// Goccia.Keywords.Contextual.pas
const
  KEYWORD_GET          = 'get';
  KEYWORD_SET          = 'set';
  KEYWORD_TYPE         = 'type';
  // ... more contextual keywords
```

**Why dedicated units?**

- **No magic strings** — the lexer, parser, AST report and package crawler reference `KEYWORD_THIS` or `KEYWORD_GET` instead of `'this'` or `'get'`.
- **Reserved vs contextual** — reserved keywords always produce a dedicated token type and cannot be used as identifiers; contextual keywords have special meaning only in specific syntactic positions (e.g., `get`/`set` in object literals, `type` in declaration position) but are otherwise valid identifiers.
- **Minimal dependencies** — neither unit has a `uses` clause, so any unit can import them without introducing circular references.

### Inlining Hot-Path Routines

The engine's units compile with `{$INLINE ON}`, and the small routines every instruction or native uses are `inline`: the value helpers of `Goccia.Core`, `PushRoot` / `RootAt` / `PopRoots`, `WriteBarrier`, the per-unit `Arg`, and the VM's frame-bound check (`FramesFull`). Routines on cold paths are marked `noinline` so they stay out of their callers: the abort of a spent limit (`XAbort`), the refusal of a memory limit (`DeliverMemoryRefusal`), a bad operand in code read from a file (`XBadOperand`).

**Why selective inlining?**

- **Only small leaves** — an inlined body that calls out or holds managed values brings its frame into the caller (see [Managed Locals on Hot Paths](#managed-locals-on-hot-paths)).
- **Not into `Execute`** — `Execute` handles the whole instruction set, so FPC keeps its dispatch variables (PC, the registers' base, the code) in memory. The hot instructions also run in `FastLoop`, a small function that makes no calls and allocates nothing, so its loop variables stay in machine registers; `Execute` enters it and takes back whatever it does not handle ([Engine design](engine/design.md)). Measure before adding a body to either.
- **Threadvar access across units is not inlined** — see [Thread Variables on Hot Paths](#thread-variables-on-hot-paths).

### Managed Locals on Hot Paths

A procedure that declares a managed local — a `string`, a dynamic array, an interface, or a record holding one — pays for it on every call, whether or not the path taken uses it: FPC initializes the local on entry, installs an implicit exception frame (`fpc_pushexceptaddr`, `fpc_setjmp`), and finalizes it on exit. The same applies to a managed temporary the compiler creates for an expression such as `IntToStr(N)` or a string concatenation, and to a function that returns a managed value.

The engine's rules ([Engine § Working rules](engine/README.md#working-rules)):

- Keep strings, dynamic arrays and other managed temporaries out of hot functions and out of `Execute` and `FastLoop`. Put the cold path, which builds the error message, in a `noinline` helper, and check the hot function's disassembly for `fpc_pushexceptaddr`.
- The dispatch loop has no Pascal `try`: JavaScript exceptions unwind through the VM's own handler records, and natives report failure through their Boolean result and `Pending`, never by raising.
- A dynamic array in a threadvar is copied by reference, with a `try`/`finally`, on every access. Hot per-thread buffers are plain memory instead: the VM's open upvalues (`OpenUpvals`) are a buffer of `OpenCap` values, not a dynamic array, for that reason.
- Error messages are built only on the path that throws: `ThrowTypeError('...' + Name)` belongs in the branch that fails, or in a helper it calls.

### Thread Variables on Hot Paths

Every piece of engine state is a threadvar ([Architecture § Evaluation Context](architecture.md#evaluation-context)), so threadvar access is on every hot path. The engine builds with FPC 3.3.1 from fpc-toolchain with native TLS threadvars, under which a threadvar costs about what a global does: `Goccia.Engine` records that single-thread runs execute 0 to 0.7% more instructions than with globals. The baseline JIT reads threadvars `%fs`-relative, as FPC's own code does.

Native TLS has had code-generation bugs, and these rules keep the engine clear of them:

- A threadvar that another unit reads goes through a `noinline` routine of the unit that declares it (`ActiveProfile`, `FirstInEngine`, `GCEpochNow`, `GCIsYoung`): an inline threadvar access in another unit can get an absolute relocation instead of a TLS offset.
- Each threadvar read in a statement of its own, where an expression would read two.
- For a threadvar array, pass the address of element 0 (`AddRootRange(@A[0], N)`), never `@A[K]` with a constant `K <> 0`.

On Linux x86-64 the `goccia` build fails when `scripts/engine/tlsscan.py` finds a threadvar address with an absolute relocation ([Engine § Build](engine/README.md#build)).

Other threads reach an engine only through what it hands them. The limit timer (`TLimitTimer` in `Goccia.VM`) holds pointers to its engine thread's `Interrupt` and `FrameLimit`: when the deadline passes it sets `Interrupt` and drops the frame bound to 0, so the frame check every call already makes, and the `Interrupt` poll at every back edge, end the run with no extra cost per call. State that every thread reads cheaply and few write is a process-wide `var`: `MultiRealmAny` tells `FastLoop` that some thread has a second realm, where the threadvar `MultiRealm` would cost a lookup.

### Singleton Special Values

Special values like `undefined`, `null`, `true`, `false` and the array hole are constants of `TValue`, not objects:

```pascal
VUndefined = TAG_SPECIAL or 0;
VNull      = TAG_SPECIAL or 1;
VFalse     = TAG_SPECIAL or 2;
VTrue      = TAG_SPECIAL or 3;
VHole      = TAG_SPECIAL or 4;
```

**Why constants?**

- **Identity comparison** — `V = VUndefined` is one `QWord` compare.
- **Memory efficiency** — nothing is allocated, rooted or pinned for them.
- **Semantic correctness** — there is only one `undefined` in JavaScript; the representation makes that true by construction.

`NaN`, the infinities and `-0` are doubles stored in the value, like every other Number; every boxed NaN is the canonical one (`CANON_NAN`), so a NaN produced by arithmetic is canonicalized when it is boxed (`MkArith`).

### One Engine per Thread

Every piece of engine state — heap, atoms, shapes, value and frame stacks, built-ins, realms, module records, job queue, host hooks — is a threadvar (`Goccia.Engine`). A thread that calls `EngineInit` has an engine of its own and shares nothing with the others, so values never cross threads, and nothing is locked on the hot paths.

- **One `TGocciaEngine` per thread at a time.** A host that runs several files at once runs each on its own thread; a nested run (a sandbox `runScript`) runs on a new thread while its caller waits.
- **Process-wide `var`s hold only what every engine shares**: the hooks units install to reach later units (the same routine for every engine), settings a host fixes before any engine starts (`EngineNurseryBytes`), and state that is shared by design and guarded (`HeapExternalBytes`, the bytes of SharedArrayBuffer blocks; `Goccia.Shared`'s waiter lists under one critical section).
- **Once-per-engine set-up** keeps the generation it ran for (`FirstInEngine`, `SameEngine`), so a thread's next engine runs it again.

### Configurable Host Surface

`EngineInit` always makes the core language built-ins (ECMA-262, ECMA-402, Temporal, ShadowRealm). Everything else is the host's, decided by `TGocciaEngine`'s properties before `PrepareHost`:

- **CLI host** — `InstallHostGlobals` (`Goccia.HostAPI`) runs the host preludes: console, URL, web APIs, events, fetch, JSON5, CSV/TSV, JSONL, YAML/TOML, SemVer, the `Goccia` object, timers and `node:async_hooks`, and in sandbox mode `fs`, `goccia` and `goccia:search`. It removes what the conformance host keeps and the CLIs leave out (`print`, `eval`, `escape`, `unescape`, and `ShadowRealm` unless the profile enables it).
- **Bare** — `TGocciaEngine.Bare` keeps GocciaScriptLoaderBare's surface: the core built-ins, `DOMException`, `atob`, `btoa`, `PermissionDenied`, `print` and `Goccia`, with no module content provider.
- **Testing** — GocciaTestRunner sets `TestMode` and calls `InstallTestingAPI`, which runs `prelude/testing.js` (`describe`, `test`, `expect`, the hooks, `mock`, `spyOn`) and registers `goccia:test`.
- **Benchmarking** — GocciaBenchmarkRunner sets `BenchMode` and calls `InstallBenchmarkAPI`, which provides the `goccia:microbench` module (`bench`, `group`, `run`, `summary`, `boxplot`).
- **FFI** — `Goccia.FFI` installs the `FFI` global only when `TGocciaEngine.Capabilities` grants `ffi`, which CLI tools reach through `--allow-ffi` or `"allow-ffi"` in a trusted config file's `permissions` block (`-P` for one run, or `--trust`; see [Config trust](permissions.md#config-trust)).
- **Timers** — `ExposeTimers` (on by default) gives the timer globals and `goccia:timers`; without them the queue still serves the host's own users (`AbortSignal.timeout`).

### Capability Checks

Anything that reaches outside the process asks the engine's capability set ([Permissions](permissions.md)), fixed when the host sets `TGocciaEngine.Capabilities` before the run. Read it from the engine that owns the operation; never cache a decision in process-wide state, since engines on other threads run with other sets. A `fetch` request carries its engine's set to the worker thread that performs it (`Goccia.Fetch`), which checks it again on every hop and against every resolved address. A denial throws the error `NewPermissionDenied` (`Goccia.VM`) makes, naming the capability and the guest-visible scope, with the host's suggestion in a slot the script cannot read; each decision is reported through `AuditDecision`, which reaches the host's `TGocciaEngine.AuditSink`.

### JavaScript Preludes

Built-ins whose specification steps matter more than their speed are written in JavaScript: the RegExp spec layer (`prelude/regexp.js`), iterator helpers, explicit resource management, `Array.fromAsync` and the grouping and `getOrInsert` methods, Intl's objects and option processing, and Temporal. Each engine prelude is a function expression that `CompilePrelude` (`Goccia.Snapshot`) compiles and its unit calls once per realm with the global object and the natives it needs:

```javascript
(function (G, N) {
  "use strict";
  // 22.2 RegExp by the spec's steps ... Goccia.RegExpV keeps native fast paths for an unmodified RegExp
  // and calls these otherwise. N holds the natives: exec (RegExpBuiltinExec), isRE, alloc, init, source, flags.
  var apply = Reflect.apply, construct = Reflect.construct, defineProperty = Object.defineProperty;
  // ...
```

- **Capture intrinsics at the top.** A prelude reads `Reflect.apply`, `Object.defineProperty` and the like once, so a script that replaces the globals later does not change the built-in's behaviour.
- **Snapshot.** The build compiles the preludes with the freshly built program and embeds their bytecode in the binary (`Goccia.Snapshot`); at start-up a snapshot entry is used only when the prelude's source still hashes to the stored value, and a nested function's body is read on its first call. The host preludes in `Goccia.HostAPI` (`console.js`, `timers.js`, `fetch.js`, `testing.js`, ...) take `(G, n, shared)`: the global object, the host's natives and an object of internals they share.
- **Native fast paths.** A hot API backed by a prelude, such as RegExp, keeps a native fast path guarded on the objects involved being unmodified ([Engine design § Built-ins](engine/design.md#built-ins)).
- **Built-in classes.** A prelude marks its classes with `markIntrinsic`, so a construction whose `new.target` has no object `prototype` falls back to the class's own prototype in the constructor's realm, as a built-in constructor's does.
- **Host code is not the script's.** Host prelude sources are named like `<goccia:console>`; the compile profile leaves them out (`ExcludeHostSource`), and the runtime profile charges their time and allocations to the caller.

The legacy globals (`parseInt`, `parseFloat`, `isNaN`, `isFinite`) are natives of `Goccia.Builtins`, and `Number.parseInt` and `Number.parseFloat` are the same function objects as the globals, as ES2026 §21.1.2.12 and §21.1.2.13 require. A new Annex B surface goes behind an off-by-default compatibility flag rather than into the default profile; see [ADR 0177](adr/0177-annex-b-is-part-of-the-test262-conformance-target.md).

### String Interning — Attempted and Rejected

What was rejected is interning every string value: the old engine cached string values in a dictionary keyed by content and measured a net regression, because hashing a string costs more than allocating a short-lived one and most strings computed at run time are unique ([ADR 0013](adr/0013-reject-string-interning.md)). The engine does not intern string values either. A concatenation, a substring or a number's string form is a new `ckString` cell, and two strings compare by content (`StrEquals`, `StrCompare`). Long concatenations build growable views over a shared buffer (`ckStrBuf`), and a long substring shares its parent's characters (`cfSlice`), which cuts copying rather than allocation.

What the engine interns is property keys. An atom is the one string for a given text (`Atomize`, `AtomOf`, `AtomOfChars`, `IntAtom` for an integer's decimal text), kept outside the collected heap; shapes and inline caches compare keys by atom pointer. The scanner interns identifiers straight from the source (`AtomOfChars`), and a string cell caches its hash (`TStr.Hash`) and, once it is known, its atom (`TStr.Atom`). Symbols and private names are key atoms no source text can name (`NewKeyAtom` with `kfSymbol` or `kfPrivate`).

Numbers need no cache at all: they are stored in the value. [ADR 0081](adr/0081-reject-value-caches-for-allocation-reduction.md) records the old engine's measurements of value caches.

## Related documents

- [Architecture](architecture.md) — Pipelines, main layers, design direction, duplication boundaries
- [Bytecode VM](bytecode-vm.md) — Register VM and bytecode format
- [Value system](value-system.md) — Values and property access
- [Garbage collector](garbage-collector.md) — The heap and collection
- [Engine](engine/README.md) and [Engine design](engine/design.md) — The engine's status, build, hosts and design
- [Contributing](../CONTRIBUTING.md) — Workflow and Pascal code style when you modify the repository
