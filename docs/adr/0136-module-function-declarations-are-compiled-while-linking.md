# 0136 - A module's function declarations are compiled while it is linked, and read its imports by name

**Date:** 2026-10-13
**Area:** `modules`, `bytecode compiler`, `bytecode VM`
**Related:** [ADR 0014](0014-bytecode-and-interpreter-feature-parity.md), [ADR 0134](0134-direct-eval-compiles-as-a-closure-of-its-call-site.md)

## Context

ES2026 §16.2.1.7.3.1 InitializeEnvironment creates a module's function
declarations while the module is linked, before any module body runs, with
the module environment as their environment. A module in an import cycle can
therefore call another module's functions before that module's body has run.

The module loader did this with the tree-walk evaluator
(`HoistFunctionDeclarations`), in bytecode mode too. Bytecode compilation of
the module body happened later, when the module was evaluated, and read the
exported declarations from the environment instead of compiling them. Those
functions, and everything nested in them, tree-walked on every call, and the
VM had to carry code that hands objects between the two executors
([#1342](https://github.com/frostney/GocciaScript/issues/1342)). This coupling
has to go before the interpreter can be removed
([#825](https://github.com/frostney/GocciaScript/issues/825)).

The problem is ordering. The functions must exist at link time, but the body
whose compilation produces them used to be compiled only at evaluation time.
A closure created at link time also has no module-body frame to capture. The
body keeps a module's imports in registers of that frame, and the closures
the body creates capture them as upvalues.

## Decision

- **The executor instantiates a module's function declarations.** The loader
  calls `TGocciaModuleLoader.InstantiateModuleFunctions` where it used to call
  the evaluator. `TGocciaEngine.Execute` makes the same call for its entry
  module. The interpreter keeps `HoistFunctionDeclarations` behind this hook.
  Module linking no longer calls `Goccia.Evaluator`.
- **The bytecode executor compiles the module while it is linked.** Each
  top-level function declaration, `export default function` included,
  compiles to a template of the top-level template, marked with the binding it
  initializes (`TGocciaFunctionTemplate.ModuleEnvironmentBinding`). The
  executor creates each function from its template in the module environment
  (`TGocciaVM.InstantiateModuleFunction`). It keeps the compiled module in the
  evaluation context, and the body runs that module later. The body reads each
  declaration's function from the environment instead of creating it again,
  so a function has one identity from linking on.
- **A function created while linking reads its module's imports by name.**
  Every other top-level binding of a linked module is already read by name
  from the module environment. A link-time function's scope now does the
  same for an import (`TGocciaCompilerScope.ImportUpvaluesByName`). If such a
  function would still capture anything from the body's frame, the compiler
  raises an internal error. Its upvalues are therefore never read, and the VM
  gives them cells of their own.
- **The module environment holds every import a link-time function can
  read.** Linking already binds evaluation-phase imports. `Execute` binds the
  entry's as each requested module loads. A source-phase or deferred import
  is bound when its declaration runs, as the interpreter binds it.
- **An import read by name is cached at its read site.** `OP_GET_GLOBAL`
  caches the environment's import binding object. The cache is validated by
  the environment's lexical-map stamp, which changes whenever an import
  binding of the map is created or replaced.

The template field and the evaluation-context field are runtime-only. A
loader-linked module is compiled in process and never serialized, so the
`.gbc` format does not change.

## Consequences

- No function of a module tree-walks in bytecode mode. An exported function's
  calls run on the VM, and so do the classes and closures it creates.
- A compile-time error of an imported module is raised while the module is
  linked, before any module of the graph evaluates, instead of when the
  module is evaluated.
- In `TGocciaEngine.Execute` with a bytecode executor, an exported function
  of the entry module now reads the entry's imports. It used to throw
  `ReferenceError` for the import's temporal dead zone even after the body
  had run.
- A link-time function's import read costs a cached binding lookup rather
  than an upvalue load followed by a namespace read. Measured, it costs less.
- `TGocciaClassValue.UsesOwnInstantiation`, `TryConstructOnReceiver` and the
  VM's construct redirect stay for classes that other evaluator-run code
  builds, the bootstrap shims, until the interpreter is removed.
