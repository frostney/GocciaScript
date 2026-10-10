# 0134 - Direct eval compiles to a closure of its call site, and a sloppy eval's new vars live in a hidden variable object of the calling function

**Status:** Proposed
**Date:** 2026-10-10
**Area:** `bytecode runtime`, `compiler`, `eval`, `ShadowRealm`
**Related:** [#872](https://github.com/frostney/GocciaScript/issues/872), epic [#825](https://github.com/frostney/GocciaScript/issues/825), [#875](https://github.com/frostney/GocciaScript/issues/875), [#874](https://github.com/frostney/GocciaScript/issues/874), [#1342](https://github.com/frostney/GocciaScript/issues/1342), [#1433](https://github.com/frostney/GocciaScript/pull/1433), [ADR 0005](0005-register-based-bytecode.md), [ADR 0048](0048-opt-in-non-strict-compatibility.md), [ADR 0085](0085-defer-annex-b-before-1-0.md), [ADR 0131](0131-bytecode-frame-positions-at-capture.md)

## Context

The bytecode VM still runs eval code with the tree-walk evaluator. This is the
VM's last dependency on the evaluator that blocks deleting the interpreter
(#825). There are three entry points:

- **Direct eval.** `OP_CALL` with `CALL_FLAG_DIRECT_EVAL` reaches
  `TGocciaVM.ExecuteDirectEval`, which parses the source and calls
  `EvaluateEvalProgram`.
- **`ShadowRealm.prototype.evaluate`** and a child realm's `eval` call
  `PrepareEvalProgram`, `RunEvalProgramBody` and `EvaluateEvalProgram` on the
  child engine's interpreter, whatever the mode
  (`Goccia.Builtins.GlobalShadowRealm.pas`).
- **The Test262 host's `eval` function**, called indirectly, does the same in
  `TTest262EvalHost.Eval` (`Goccia.Test262.Host.pas`).

A global `eval` exists only under the Test262 host, and in a ShadowRealm child
of a realm that has one. Ordinary realms have none.

### How the bridge works today

Caller bindings live in registers, which the evaluator cannot see. The bridge
therefore has three parts:

- **A compile-time snapshot.** `CaptureDirectEvalEnvironment` (in
  `Goccia.Compiler.Expressions.pas`) records each name visible at the call in a
  `TGocciaDirectEvalEnvironment`, with its kind and index: `debLocal` slot,
  `debUpvalue` index, `debGlobal`, or a `with` object.
- **A run-time adapter.** `TGocciaVMDirectEvalScope` is a `TGocciaScope`
  subclass. It reads and writes those slots in the VM's current frame
  (`BindingValue` calls `FVM.GetLocal(Index)`).
- **Copy-back.** After the eval, `CopyBackVariableBindings` and
  `CopyNewVariableBindingsToParent` move var bindings back.

A var that a sloppy eval creates lands in a lazily created per-frame
`TGocciaScope`, `FCurrentDynamicVarScope`. Closures capture it
(`TGocciaBytecodeClosure.DynamicVarScope`), and `OP_GET_UPVALUE`,
`OP_GET_GLOBAL`, `OP_HAS_GLOBAL`, `OP_SET_UPVALUE_DYNAMIC` and
`OP_RESOLVE_UPVALUE_REF` consult it at run time. Everything an eval creates
(functions, classes, generators) is AST-backed, even in bytecode mode.

### Measured behavior of the bridge

Measured on `main` at `0a7b3166`, Linux x86-64, FPC 3.2.2, with
`GocciaTest262Runner --eval-host`. The reference is Node.js 24.21.

| Probe | Bytecode | Interpreter | Node.js |
|---|---|---|---|
| Second eval in the same function reads a var the first eval created (`eval("var s='inner'"); eval("s")`) | `outer` | `inner` | `inner` |
| A static field initializer's eval is strict | `false` | `true` | `true` |
| `{ eval("var X"); let X; }` | no error | `SyntaxError` | `SyntaxError` |
| `function g(X) { eval("var X = 2"); return X }` | `SyntaxError` | `SyntaxError` | `2` |
| `catch (X) { eval("var X = 2") }` | `SyntaxError` | `SyntaxError` | `2` (web-compat step) |
| A closure created by eval, called after its frame returned | **another function's source text** | `1` | `1` |
| The same closure, writing the caller's var after return | `TypeError` | `7` | `7` |
| `eval("#x in o")` in a static method, `o` an instance | `false` | `true` | `true` |

The outlived-closure rows are not stale values. They read whatever frame is
current when the closure runs, because the adapter addresses the caller by
slot number in "the current frame". The other bytecode rows follow from code
read in the same commit:

- **Second eval.** `TryGetBinding` answers from the call-site snapshot before
  it reaches the parent scope that holds the first eval's var.
- **Static field initializer.** The VM passes `Template.StrictCode`, which
  describes the whole function. Class-body code runs inline in that function,
  so the eval gets the function's strictness, not the class body's.
- **`let` after eval.** `TGocciaVMDirectEvalScope.Create` declares only caller
  bindings that are already initialized. A `let` still in its temporal dead
  zone never reaches the conflict check.

The regression coverage the issue requires is the #1310 and #1303 sections of
`scripts/test-cli-apps.ts`, and both pass today.

### What the bridge costs code that never calls eval

A throwaway spike, never committed, removed five hooks from the dispatch loop
and the call path:

- the `FCurrentDynamicVarScope` test in `OP_GET_UPVALUE`;
- the same test in `OP_GET_GLOBAL`;
- the same test in `OP_HAS_GLOBAL`;
- the per-call store in `SetupNewFrame`;
- the per-closure `DynamicVarScope` store and `FDynamicVarUpvalues` array in
  `OP_CLOSURE`.

Valgrind callgrind instruction counts, whole run, `./build.pas --prod runner`,
bytecode mode, base `0a7b3166`:

| Workload | Base | Hooks removed | Change |
|---|---:|---:|---:|
| Closure creation with captured upvalues, upvalue and global reads (scratch script) | 3,974,799,929 | 3,893,724,202 | -2.04% |
| `perf/probes/fixed-arg-call.js` | 94,221,851 | 93,971,313 | -0.27% |
| `perf/probes/method-call-fixed-arg.js` | 107,404,844 | 107,155,531 | -0.23% |
| `perf/probes/array-callbacks.js` | 533,127,794 | 532,467,689 | -0.12% |
| `perf/probes/native-builtin-call.js` | 898,486,857 | 898,385,929 | -0.01% |
| `perf/probes/nbody-minimal.js` | 162,906,015 | 162,885,501 | -0.01% |
| `perf/probes/fib-recursive.js`, `generic-plus-scalars.js`, `propaccess-monomorphic.js` | | | 0.00% |

Three more costs fall on code without eval:

- **Realm-wide register-read deopt.** A realm that hosts direct eval sets
  `TGocciaRealm.HostsDirectEval`, and the executor passes it to the compiler
  as `DirectEvalAvailable`. That switches off direct register reads of `let`
  bindings and parameters in every function of the realm, because "a closure
  created by direct eval writes registers by slot in whichever function calls
  it" (`docs/bytecode-vm.md`).
- **Constant-folding deopt in sloppy code.** PR #1360 stops constant folding
  and type trust across any non-strict function boundary, because the compiler
  cannot tell which sloppy functions contain an eval
  (`TGocciaCompilerScope.DirectEvalMayShadow`).
- **A flag test on every call.** `OP_CALL` tests the direct-eval flag bit on
  every call.

## Specification

The citations are to ECMA-262 2026, read through tc39-mcp 0.6.3 at snapshot
`0248456c758431e4bb8e5d26333ff1865123c9cd` (es2026). The draft at
`5345883164f463e87f8b40aca4956157ecba8783` (main) was also consulted.

| Clause | What the design depends on |
|---|---|
| §13.3.6.1 Function Calls, Runtime Semantics: Evaluation (`sec-function-calls-runtime-semantics-evaluation`) | A call is direct eval only when its callee is the unqualified Reference `eval` and `SameValue(func, %eval%)`. `strictCaller` is `IsStrict` of *that CallExpression*, not of the enclosing function. |
| §19.2.1.1 PerformEval (`sec-performeval`) | **Early errors.** `inFunction`, `inMethod`, `inDerivedConstructor` and `inClassFieldInitializer` come from `GetThisEnvironment()` and decide the early errors for `new.target`, `super.x`, `super()` and `arguments`. **Environments.** A direct eval gets `lexEnv = NewDeclarativeEnvironment(caller's LexicalEnvironment)`, `varEnv` = the caller's VariableEnvironment, and `privateEnv` = the caller's PrivateEnvironment. **Strict eval.** `varEnv := lexEnv`. **Indirect eval.** It uses the realm's global environment and a null `privateEnv`. |
| §19.2.1.3 EvalDeclarationInstantiation (`sec-evaldeclarationinstantiation`) | **Global case (step 3.a).** In a sloppy eval whose varEnv is global, a var name that names a global lexical declaration is a `SyntaxError`. **Intervening scopes (step 3.d).** A var name bound by any declarative environment between `lexEnv` and `varEnv` is a `SyntaxError`. A normative-optional web-compat branch exempts a `Catch` clause's environment. **Private names (steps 4-7).** `AllPrivateIdentifiersValid` runs against every enclosing PrivateEnvironment. **Global checks.** Global vars and functions use `CanDeclareGlobalFunction` and `CanDeclareGlobalVar`. **Deletable bindings.** Function and var bindings are created with `CreateMutableBinding(name, true)`, so they can be deleted. |
| §10.2.11 FunctionDeclarationInstantiation (`sec-functiondeclarationinstantiation`) | **Parameter expressions (step 20).** With parameter expressions, a sloppy function gets a separate environment so that "bindings created by direct eval calls in the formal parameter list are outside the environment where parameters are declared". **Separate top-level lexical environment (step 32).** A sloppy function gives its top-level lexical declarations their own environment "so that a direct eval can determine whether any var scoped declarations introduced by the eval code conflict". |

## How other engines implement direct eval

Each engine was read from its own source on 2026-10-10:

- V8 at `v8/v8@main`;
- SpiderMonkey at `mozilla-central` tip, under `js/src/`;
- JavaScriptCore at `WebKit/WebKit@main`, under `Source/JavaScriptCore/`;
- QuickJS at `bellard/quickjs@master`;
- Boa at `boa-dev/boa@main` and Hermes at `facebook/hermes@main`, as extra
  data points.

Line positions drift, so functions are named instead.

**V8.**

- **Detection.** `ParserBase::CheckPossibleEvalCall` (`src/parsing/parser-base.h`)
  calls `Scope::RecordEvalCall` (`src/ast/scopes.h`). It sets `calls_eval`, marks
  the declaration scope `sloppy_eval_can_extend_vars` for sloppy code, and
  sets `inner_scope_calls_eval` on every outer scope.
- **Caller locals.** `Scope::MustAllocateInContext` (`src/ast/scopes.cc`) ends
  in `return var->has_forced_context_allocation() || inner_scope_calls_eval();`.
  Every binding of the calling function and its enclosing functions therefore
  moves into a heap `Context`.
- **Eval code.** It is compiled against the caller's serialized `ScopeInfo`
  (`Compiler::GetFunctionFromEval`, `src/codegen/compiler.cc`), so names
  resolve to context slots at compile time.
- **Sloppy injection.** A sloppy `var` in eval becomes `DeclareEvalVar` /
  `DeclareEvalFunction` (`DeclareEvalHelper` in `src/runtime/runtime-scopes.cc`).
  This lazily creates an extension object with
  `NewJSObject(context_extension_function())` and `context->set_extension(...)`.
- **Names past a sloppy-eval scope.** They compile to `LdaLookupContextSlot` or
  `LdaLookupGlobalSlot` (`Scope::LookupSloppyEval`). These take a fast path
  while no context on the path has an extension.
- **Cache.** `CompilationCache::LookupEval`, keyed on source, outer
  `SharedFunctionInfo`, language mode and position.

**SpiderMonkey.**

- **Detection.** For an `eval` callee, `Parser.cpp` calls
  `setBindingsAccessedDynamically()` and `setHasDirectEval()`, and
  `setFunHasExtensibleScope()` in sloppy functions.
- **Caller locals.** `allBindingsClosedOver()` returns
  `bindingsAccessedDynamically()` (`frontend/SharedContext.h`), so every binding
  lives in a `CallObject` or `LexicalEnvironmentObject`.
- **Eval code.** It resolves names at compile time by walking the live
  environment chain (`ScopeContext::searchInEnclosingScopeNoCache`,
  `frontend/Stencil.cpp`) to `EnvironmentCoordinate` hops and slots.
- **Dynamic names.** A function with an extensible scope makes free names
  dynamic (`fallbackFreeNameLocation_ = Dynamic()`, `frontend/EmitterScope.cpp`).
- **Sloppy injection.** Sloppy vars go onto the nearest qualified var object
  through `GlobalOrEvalDeclInstantiation` (`vm/EnvironmentObject.cpp`).
- **Cache.** `EvalCache` (`builtin/Eval.cpp`), keyed on string, caller script
  and pc.

**JavaScriptCore.**

- **Detection.** `ASTBuilder::makeFunctionCallNode` (`parser/ASTBuilder.h`)
  builds an `EvalFunctionCallNode` and sets `EvalFeature`. The parser marks
  scopes that use `eval` and propagates that to parents.
- **Caller locals.** `markAllVariablesAsCaptured()` and
  `shouldCaptureAllOfTheThings = ... || usesEval()`
  (`bytecompiler/BytecodeGenerator.cpp`) put every binding in a
  `JSLexicalEnvironment`.
- **Eval code.** It uses `op_resolve_scope` / `op_get_from_scope`, linked
  against the real scope chain (`JSScope::abstractResolve`, `runtime/JSScope.cpp`).
- **Sloppy injection.** `Interpreter::executeEval` puts sloppy vars on the
  nearest var scope and fires a realm-wide `varInjectionWatchpointSet`.
  Callers' global accesses carry `...WithVarInjectionChecks`.
- **Cache.** `DirectEvalCodeCache` (`bytecode/DirectEvalCodeCache.h`), keyed on
  source and bytecode index.

**QuickJS** (`quickjs.c`).

- **Detection.** The parser turns an `eval(...)` call into `OP_eval` and sets
  `has_eval_call`.
- **Hidden bindings.** `add_eval_variables` adds hidden `this`, `new.target`,
  home-object and `arguments` variables.
- **Caller locals.** It calls `capture_var` on every argument and var, and
  walks every enclosing function to capture its in-scope variables as closure
  variables.
- **Hidden variable object.** In sloppy code it adds a hidden local `_var_`
  (`s->var_object_idx = add_var(ctx, s, JS_ATOM__var_)`), and `_arg_var_` when
  there are parameter expressions. Function entry fills it with a
  null-prototype object (`OP_special_object OP_SPECIAL_OBJECT_VAR_OBJECT`).
- **Eval code.** `__JS_EvalInternal` takes the caller's
  `JSFunctionBytecode` and calls `add_closure_variables`. That rebuilds the
  eval function's closure variables from the caller's `vardefs`, starting at
  the call's scope index. `js_closure` then instantiates it against the live
  frame.
- **Name resolution.** `resolve_scope_var` resolves the function's own scopes
  statically. A name it does not find there is tested against `_var_` with the
  same machinery it uses for `with` (`OP_with_get_var`). `_var_` and `_with_`
  are handled by one branch.
- **Cache.** None. The source says "eval performance is less critical".

**Others.**

- **Boa.** It escapes every binding of a function that contains direct eval
  (`core/ast/src/scope_analyzer.rs`) and compiles eval against the caller's
  compile-time scope.
- **Hermes.** It lists local-mode `eval()` as unsupported
  (`doc/Features.md`).

| | V8 | SpiderMonkey | JavaScriptCore | QuickJS |
|---|---|---|---|---|
| Detection | Static, parser (`RecordEvalCall`) | Static, parser (`setHasDirectEval`) | Static, parser (`EvalFeature`) | Static, parser (`has_eval_call`) |
| Caller locals of an eval function | Heap `Context` slots | `CallObject` / lexical env | `JSLexicalEnvironment` | Captured `JSVarRef` boxes |
| Eval-code names | Compile time, against `ScopeInfo` | Compile time, against the env chain | Linked at run time | Compile time, against `vardefs` at the call's scope index |
| Sloppy var injection | Lazy extension object on the `Context` | Properties of the var object | Properties of the var scope, plus a realm watchpoint | Properties of the hidden `_var_` object |
| Caller names an eval may shadow | Lookup slot, fast while no extension exists | Dynamic name ops | `WithVarInjectionChecks` | `with`-style probe of `_var_` |
| Cost to functions without eval | None; enclosing functions pay | None; enclosing functions pay | None, apart from the realm watchpoint | None; enclosing functions pay |
| Eval code cache | Yes | Yes | Yes | No |

No engine lets eval code address the caller's registers by slot at run time.
Each one moves an eval-calling function's bindings into boxes the eval code
captures, decides that statically, and compiles eval code against a
description of the caller kept at the call site.

## Considered options

| Criterion | 1. REPL global-backed bindings in an eval-calling function | 2. Heap environment, names resolved by the eval chunk at run time | 3a. Slot map read at run time (today) | 3b + hidden var object: slot map read at compile time, var injection into a hidden object (**chosen**) |
|---|---|---|---|---|
| Sloppy `var` leaks into the function | Yes: the function's scope holds it | Yes: an extension on the environment | Partly; a second eval misses the first eval's vars | Yes: a property of the hidden variable object, probed by every reference that can see it |
| `let`/`const` stay inside the eval | Only with a scope object per block, which the REPL path does not have | Yes | Yes | Yes: they are locals of the eval template |
| A closure from eval captures caller bindings | Yes, by name | Yes | **No**: reads the current frame by slot | Yes: ordinary upvalue cells |
| Functions created in eval are bytecode | Yes | Yes | No | Yes |
| Private names, `super`, `new.target`, `arguments` | Need a name-keyed private environment | Need name-keyed lookups for hidden bindings | Separate adapter code per feature | Compiled the same way as an arrow at the call site |
| Generator or async caller, eval mid-frame | Scope objects survive suspension | Same | Needs `FContinuationDynamicVarScope` | Nothing special: cells and the hidden local are frame state the continuation already saves |
| Cost to functions without eval | None, if decided statically | None, if decided statically | Not zero: see the measurements above | None |
| Cost to a function that contains eval | Every local access becomes a named lookup | Eval code does a named lookup per access | Cell-safe reads only | Cell-safe reads only; sloppy callers also probe free names |
| Size and risk here | Rebuilds the interpreter's environment model in the VM, as #875 is removing it | A run-time name to cell table, plus dynamic ops | Already present, unsound | Reuses the nested-function compiler and the `with` probe pattern; one new record and a few opcodes |

**Option 1** fits only the global case. When an eval's variable environment
is the global environment, the bindings are already name-backed:

- indirect eval;
- `ShadowRealm.prototype.evaluate`;
- direct eval in a script's global code.

The decision uses a variant of it there (see below). For a function, the REPL
mechanism keeps only *top-level* bindings named. Block scopes, catch
parameters, loop bindings and `with` would each need a named scope object, and
every access in the function and in the enclosing functions it can see would
become a hash lookup. That re-creates the evaluator's environment chain inside
the VM, at the moment #825 is deleting it.

**Option 2** is what SpiderMonkey and JSC do for names inside a sloppy eval
function. It is correct, but the eval code would resolve every name at run
time even though the caller already knows each binding's slot at compile
time. It also needs a run-time name to cell table per frame, data the
call-site record already holds.

**Option 3a** is the current bridge. The outlived-closure rows of the measured
table show that a closure from eval reads the frame that happens to be current, so the model is unsound
whenever an eval closure outlives or leaves its frame. No amount of copy-back
fixes that.

**Option 3b alone** compiles eval names against the slot map at compile time,
so they become captures of the caller's cells. That is QuickJS's
`add_closure_variables`. It does not handle a sloppy `var` that shadows a name
the caller resolved elsewhere. QuickJS adds the hidden `_var_` object for
exactly that case, and so does this decision.

## Decision

Eval code is compiled to bytecode and runs on the VM. A direct eval compiles
as a function nested at its call site, against a record of that site, and is
instantiated as a closure of the caller's live frame. A sloppy eval's new
var-scoped bindings live in a hidden variable object of the calling function.
References that such a var could shadow probe that object before their static
binding. Evals whose variable environment is the global environment share the
same compile path, in a global-code mode. The design follows QuickJS most
closely. Unlike QuickJS, it captures caller bindings lazily, because
GocciaScript's cells are already created on demand.

### Static detection

The parser marks each function body that contains a call whose callee is the
plain identifier `eval`. Optional calls and member calls do not count. The
bodies marked include functions, arrows, methods, accessors, class field
initializers and static blocks, as well as a script or module body. The parser
also records whether that call is in sloppy code. Only `SameValue(func,
%eval%)` remains a run-time question (§13.3.6.1). The compiler reads the flags
before it compiles a body, the way V8 reads `calls_eval`.

The compiler then makes two choices for the whole function, not just from the
first eval onwards:

- **Direct register reads.** A function containing direct eval reads its
  bindings through cell-aware reads.
- **Constant folding.** A binding of an enclosing function loses constant
  folding and type trust only when a function containing a *sloppy* direct
  eval lies between the reference and the declaration. This narrows #1360's
  deopt from every non-strict function to those functions.

`DirectEvalAvailable` and the compiler's use of `HostsDirectEval` are removed.
Eval code writes caller bindings through cells, like any closure, so other
functions of the realm are unaffected.

### The eval site record

Every direct-eval call compiles to a new instruction, `OP_DIRECT_EVAL`, and
`CALL_FLAG_DIRECT_EVAL` leaves `OP_CALL`. One operand indexes a per-template
eval site record. The record extends today's `TGocciaDirectEvalEnvironment`
and is serialized in `.gbc` under a format-version bump. It holds:

- **Visible bindings.** Every binding visible at the call: name, kind (caller
  slot, caller upvalue index, global-backed, or `with` object), `const`, and
  whether it is in the variable environment or in a declarative scope between
  the call and it. Block bindings still in their TDZ are included, since the
  compiler already hoists them at block entry. The `this` binding, the
  derived-constructor `this` flag and the `arguments` slot are listed as
  bindings, as they are today.
- **Caller strictness.** The strictness of this call expression.
- **Early-error flags.** `inFunction`, `inMethod`, `inDerivedConstructor`,
  `inClassFieldInitializer`, plus today's rejection of `arguments` in
  parameter lists.
- **Private names.** The private-name environment: each visible private
  identifier and the compiled key it resolves to, plus where the run-time
  private class comes from (see below).
- **Variable-environment shape.** One of:
  - global;
  - function, with the slot of the hidden variable object;
  - parameter list of a sloppy function with parameter expressions, with the
    slot of a second hidden object (§10.2.11 step 20);
  - none, for a strict call site.

### Compiling and running eval code

At the call, the VM parses the source as a Script. It then compiles the
Script with a synthetic parent compiler scope built from the record:

- caller slots become locals at their slots;
- caller upvalues become upvalues at their indexes;
- `with` objects become hidden `with` bindings;
- private names are declared with their keys.

The Script body compiles as an arrow-like function. `this`, `new.target`, the
home object, `arguments` and the private class come lexically from the call
site, as for an arrow written there. The ordinary nested-function resolver
therefore produces upvalue descriptors that name caller slots (`IsLocal`) or
caller upvalue indexes. Static early errors are compile-time `SyntaxError`s
raised by the eval call before any eval code runs:

- PerformEval's early errors;
- `AllPrivateIdentifiersValid`;
- step 3.d's conflicts with declarative scopes between the call and the
  variable environment.

The VM instantiates the template as `OP_CLOSURE` would in the caller's frame.
`GetLocalCell` creates cells for the captured slots, and the register and
cell stay coherent through `SetRegisterRaw` and `GetLocalRegister`. The VM
then pushes the closure as an ordinary trampolined frame, whose completion
value lands in the instruction's destination register. As a result:

- Functions, classes and generators created in eval are bytecode.
- Closures from eval capture caller bindings by cell. A closure that outlives
  its frame reads and writes the right binding.
- An eval inside a generator or async function needs nothing special, because
  the frame state it captures is what continuations already save.
- The eval frame is counted against `--max-stack` (#1482) and located for
  stack traces (ADR 0131) like any other bytecode frame.

### Where eval declarations go

- **Strict eval** (a strict call site, or `"use strict"` in the source). Vars,
  functions and lexical declarations are all locals of the eval template. The
  caller is unaffected.
- **Sloppy eval in a function.** For each var-declared name:
  - If the record lists a caller binding of that name in the variable
    environment (a `var`, a simple parameter, a function declaration,
    `arguments`), the declaration reuses it, and its initializer assigns
    through the captured cell.
  - Otherwise the name becomes a configurable data property of the function's
    hidden variable object. This is a null-prototype ordinary object, created
    by the first eval that needs it and stored in the hidden local. Being
    configurable, it is deletable, matching `CreateMutableBinding(name, true)`.

  Lexical declarations are template locals.
- **Global variable environment.** This covers indirect eval,
  `ShadowRealm.prototype.evaluate`, and a sloppy direct eval in a script's
  global code. Vars and functions become global bindings through the existing
  global-define instructions, with a deletable flag. The template's prologue
  runs step 3.a's lexical-conflict check and `CanDeclareGlobalFunction` /
  `CanDeclareGlobalVar`, because they depend on the global object's state.
  Lexical declarations are always template locals, so they never persist
  between evals. That is the difference from the REPL's
  `GlobalBackedTopLevel`, whose top-level lexicals are global by design.

### References an eval can shadow

Take a function F that contains a sloppy direct eval. Some identifier
references do not resolve to a binding of F's own variable environment or of
a block inside it. That covers reads, writes, `typeof`, `delete` and calls,
whether in F, in a function nested in F, or in F's eval code. Each such
reference compiles to a probe of F's hidden variable object, followed by the
static path (upvalue or global).

- **Nested functions.** A function nested in F captures F's hidden local as an
  ordinary upvalue.
- **Several eval functions.** When several such functions lie between a
  reference and its binding, the probes run from the innermost outwards, the
  same pattern the compiler already emits for `with` (`OP_HAS_WITH_BINDING`).
  QuickJS uses one code path for `_var_` and `_with_`.
- **Calls.** A call through a probed binding passes `undefined` as `this`,
  because the variable object is a declarative environment, not an object
  environment.
- **Assignments.** The reference is resolved before the right-hand side runs
  (§13.15.2), as `OP_RESOLVE_UPVALUE_REF` does today.
- **Before any var is injected.** A probe is a nil test on the hidden local.

References in functions that are not inside such an F compile exactly as
today, with no probe.

### Private names, `super` and the class body

Eval code uses the same private-name instructions as other code, against the
compiled keys in the record. At run time its closure inherits the private
class the way #1433 gives every closure one: from the running closure.

At a call site that runs inline in a class body (a computed key, a static
field initializer, an `extends` expression), #1433 hands a new closure the
class being defined with `OP_SET_PRIVATE_CLASS`. For those sites the record
instead names the register that holds the class.

The issues that come from eval taking a separate path then have nothing left
to differ on:

- [#1569](https://github.com/frostney/GocciaScript/issues/1569): `#x in o`;
- [#1574](https://github.com/frostney/GocciaScript/issues/1574): a static
  `#y in C`;
- [#1568](https://github.com/frostney/GocciaScript/issues/1568): eval in a
  computed key.

`super.x`, `super()` and `new.target` in eval behave as they do in an arrow at
the call site. A `super()` initializes the caller's `this` through the same
captured binding an arrow uses.

### Deleted by this decision

- `TGocciaVMDirectEvalScope` and its copy-back.
- `EnsureCurrentDynamicVarScope`, `FCurrentDynamicVarScope` and
  `FContinuationDynamicVarScope`.
- `TGocciaBytecodeClosure.DynamicVarScope` and its dynamic-upvalue flags, and
  `ResolveDynamicUpvalueScope`.
- The dynamic-scope branches in the opcodes measured above.
- `CollectBytecodeDirectEvalPrivateNames`, `DirectEvalLexicalThisValue` and
  the other run-time readers of the record.
- `DirectEvalAvailable`.
- The evaluator calls in `ExecuteDirectEval`, `Goccia.Builtins.GlobalShadowRealm.pas`
  and `Goccia.Test262.Host.pas`.

The VM still calls the evaluator in `InstantiateClass` for classes built by
evaluator paths. That goes with #1342, after which `Goccia.Evaluator` leaves
the VM's link graph.

## Cost model

- **Functions with no direct eval, and not nested in a sloppy-eval function:**
  identical bytecode, and less work than today. The spike above measures the
  hooks this removes, and `OP_CALL` loses the direct-eval flag test.
- **A function containing a strict direct eval:**
  - it reads its bindings through the cell-aware path;
  - its enclosing functions capture every binding visible at the call, as
    today and as in all four engines;
  - a caller slot gets a cell only if the eval code actually references it.
- **A function containing a sloppy direct eval**, plus every function nested
  in it, pays more:
  - one probe per free-name reference: a nil test until an eval injects a var;
  - one hidden local;
  - one object allocation, made only by the first eval that injects a var.
- **Each eval call** pays parse, compile, one closure allocation and one frame
  push. Today it pays parse plus an AST walk, with adapter scopes per call.
  Repeated evals of the same source at the same site are served from a
  per-site cache (Phase 4).

## Consequences

- Bytecode mode has a single scope model. The only per-name dynamic lookup
  left is the probe of a hidden variable object, and only sloppy functions
  that contain eval, and the functions nested in them, emit it.
- A missing or wrong entry in the eval site record shows up as a resolution
  error at eval compile time. There is one place to check, and test262 plus
  the eval-host sections cover it.
- The defects in the measured table become bytecode tests that should pass on
  the new path. The interpreter keeps its own eval until #875, and #825 has
  dropped mode parity as an oracle.
- Compiled eval templates are retained like `Function`-constructor modules
  (`TGocciaEngine.RetainModule`) until the open retention question below is
  settled.

### The strongest counter-argument

Option 1 or 2 would resolve every name by name, so nothing could go missing.
In this design, eval correctness depends on the eval site record mirroring
every binding the compiler can see at the site, including hidden ones: `this`,
the derived-constructor `this` flag, `with` objects, `arguments`, private
names, the class under construction. Any entry left out compiles to a global
lookup without complaint.

The answer:

- That completeness requirement exists today. The bridge reads the same
  record at run time, so its gaps already show up as wrong results (the
  measured table).
- Moving consumption to compile time puts every gap through one resolver,
  the nested-function one, which every closure already exercises.
- Every engine surveyed does the same: V8 compiles against `ScopeInfo`,
  SpiderMonkey against the environment chain, QuickJS against `vardefs`.
- The named alternatives are not free. They charge a hash lookup to every
  access in an eval-calling function and its enclosing functions, and they
  keep an environment model alive that #825 is removing.

## Implementation plan

Each phase is one pull request. Phase 3 may be a two-PR native stack. Every
phase has the same gate: no regression, test by test, against `main`, in
bytecode mode, on the pinned test262 commit, over the tests that reference
`eval(` or `ShadowRealm`.

- **Phase 1 - static facts.**
  - Parser flags for direct eval and sloppy direct eval.
  - `OP_DIRECT_EVAL` with the eval site record index, plus caller strictness,
    early-error flags and the private-name map in the record. Bytecode format
    bump.
  - `DirectEvalMayShadow` narrowed to sloppy-eval functions.
  - The bridge still runs eval, now with per-site strictness.

  Tests:
  - An eval-host section where a static field initializer, a computed key and
    an `extends` expression get a strict eval.
  - `Goccia.Compiler.Test` cases: a sloppy function without eval folds an
    enclosing `const`, and one with eval does not.
  - A `Goccia.Bytecode.Binary.Test` round trip of the record.
  - The #1310 section stays green.
  - Callgrind on the probes shows no regression.
- **Phase 2 - global-environment eval compiled.**
  - A compiler mode for eval code whose variable environment is global, with
    the prologue checks and deletable global bindings.
  - One engine-level entry that compiles and runs it on the realm's VM. It
    dispatches through the executor until #875 folds the executor into the
    engine.
  - `TTest262EvalHost.Eval` (indirect eval), the ShadowRealm child realm's
    `eval`, and `ShadowRealm.prototype.evaluate` call that entry in bytecode
    mode.
  - ShadowRealm maps compile errors to a caller-realm `SyntaxError`, and
    prologue or body errors to a caller-realm `TypeError`.

  Tests:
  - The test262 gate over `built-ins/ShadowRealm`, `built-ins/eval` and
    `language/eval-code/indirect`.
  - New eval-host sections:
    - indirect `let` does not persist between calls;
    - `delete` of an eval-created global var returns `true` and removes it;
    - a ShadowRealm evaluate with a duplicate `let` gives `SyntaxError`, and
      one with a throwing body gives `TypeError`.
  - Neither unit uses the evaluator in bytecode mode.
- **Phase 3 - direct eval compiled; bridge deleted.** Everything under
  [Decision](#decision). If it is stacked:
  - **3a** compiles direct eval for strict call sites, where no injection is
    possible.
  - **3b** adds the hidden variable object and the probes, moves sloppy call
    sites over, and deletes the bridge.

  `docs/bytecode-vm.md` and `docs/architecture.md` lose the eval coupling in
  the same PR.

  Tests: one eval-host section per row of the measured table, in bytecode
  mode, plus:
  - an eval closure called after its frame returned, reading and writing;
  - eval in a generator across `yield`, and in an async function after
    `await`;
  - an eval var seen by a closure created before the eval;
  - the #1569, #1574 and #1568 probes;
  - the #1310 and #1303 sections;
  - the JavaScript suite in both modes;
  - callgrind on the probes, against the phase-2 base.
- **Phase 4 - cache and retention.** Each eval site gets a code cache keyed on
  the source string and the site, like JSC's `DirectEvalCodeCache` and
  SpiderMonkey's `EvalCache`. The site already fixes strictness and the
  environment. The retention policy is applied. The phase is measured with a
  repeated-eval workload.

**Interactions.**

- **#1433** must land before Phase 3's private-name part. Phase 3 deletes its
  eval-only pieces: private names declared for the parse, and the private
  environment chain in the evaluator's scopes. The other session's #1574 work
  carries its tests over.
- **#874.** Bytecode mode stops producing AST-backed functions from eval, which
  removes one producer of the evaluator call paths #874 extracts.
- **#875** deletes the interpreter's own eval with the interpreter. By then
  nothing in bytecode mode uses the `TGocciaScope` subclasses the bridge
  needed.

## Open questions

1. **Retention of compiled eval code.** Phase 2 retains each compiled eval
   module for the engine's life, as the `Function` constructor already does.
   With the Phase 4 cache, repeated sources stop growing it, but distinct
   sources still do. The alternative is to make eval templates collectable
   once no closure references them.
2. **A sloppy eval `var` named like a `catch` parameter.** §19.2.1.3 step 3.d
   exempts the catch environment only under the normative-optional web-compat
   branch. GocciaScript already accepts `catch (e) { var e }` outside eval.
   ADR 0085 defers broad Annex B support. The question is whether eval should
   follow the existing non-eval behaviour or the strict reading.
3. **Is the 3a/3b split worth it?** It keeps every intermediate state on one
   environment model per call site, but costs a second review cycle.
