# 0183 - Module code clears what each top-level statement left, and an imported module keeps no completion value

**Date:** 2026-11-21
**Area:** `compiler`, `gc`, `modules`
**Related:** [ADR 0153](0153-precompiled-modules-use-the-module-linker.md), [ADR 0181](0181-a-callee-that-can-drop-a-parameter-releases-the-callers-copies.md), [#2027](https://github.com/frostney/GocciaScript/issues/2027)

## Context

[#2027](https://github.com/frostney/GocciaScript/issues/2027) made an `await`, a
`catch` and a `yield` release a value the program has dropped. Module code still
kept one in two ways, in the frame of a module's top level, which runs until the
module's body ends:

```js
// an imported module, or an entry run through TGocciaEngine.Execute
let buffer = await make();         // or: let buffer = new ArrayBuffer(…)
const length = buffer.byteLength;
buffer = null;
// a 6 MiB allocation under a 10 MiB --max-memory is refused here

await make();                      // a bare awaited expression statement
// the awaited value stays reachable until the module ends
```

- **A copy of a binding's value.** A module the loader links (an imported
  module, a `TGocciaEngine.Execute` module entry, a bundled `.gbc` module) keeps
  its top-level bindings in its Module Environment Record and reads them by name
  (`GlobalBackedTopLevel`), so `buffer.byteLength` copies `buffer` into a
  register above the live ones. The frame keeps that register's last value. No
  await is involved; #2027's clear runs only after a statement that awaited.
  `GocciaRunner`'s own entry keeps its bindings in registers and reads them in
  place, which is why it did not show this.
- **The completion value.** The top level keeps the value of its last
  expression statement in the register it returns. For an entry that is
  reported: `files[].result`, `Execute`'s result, a nested `runScript`'s
  result ([ADR 0153](0153-precompiled-modules-use-the-module-linker.md): "Entry
  completion values remain separate from promises that govern module
  evaluation"). An imported module's is never read; ECMAScript gives a module
  none (ES2026 §16.2.1.7.3.2 ExecuteModule).

## Options

1. **Clear after every statement in every frame.** Releases everything, but
   pays an instruction per register on every statement of every function and
   every loop iteration.
2. **Clear after each top-level statement of module code only.** A module's
   body runs once, so the clear runs once per top-level statement. A loop
   already clears once after itself, and the statements inside a top-level
   loop or block are not top-level ones, so no iteration pays.
3. **Read a binding for a property access without a temporary.** Removes one
   source of copies, not the others (call arguments, receivers of calls).
4. **Stop reporting a module entry's completion value.** Spec-aligned, but
   changes what hosts and the runner report today.

## Decision

**Option 2 for the copies; for the completion value, only imported modules
stop keeping it.**

- In module code (`GlobalBackedTopLevel` and `PreinitializedTopLevelFunctions`,
  not eval code), each top-level statement that completes normally clears the
  registers it used above the live ones once it ends, whether it awaited or
  not, with the same line-0 clear #2027 uses for an awaited statement
  (`EmitUncountedClearDeadRegisters`), so line coverage does not count it. A
  statement that also awaited clears once, not twice: an await in a branch
  it ends with does not clear in that branch as well, and a labeled one
  clears only after its `break label` targets. A top-level loop is
  covered by its own clear after it; a labeled statement that is not a loop
  clears after its `break label` jumps land.
- `TGocciaCompiler.DiscardsCompletionValue` compiles a top level whose
  completion value nothing reads: its expression statements compile as a
  function body's do. The engine sets it for every module but the one
  `Execute` evaluates as its entry, by role rather than by path
  (`TGocciaEvaluationContext.IsEntryModule`): a script entry that imports its
  own file loads a separate module, which keeps none. A heap snapshot writes
  that entry as a record of its own kind (`mskEntrySource`), so a restore,
  also into an engine made with another entry, compiles each module as the
  run did and the bytecode it hashes matches. The module records' schema
  version goes to 2, so a reader of either version refuses the other's
  records before decoding them.
- An entry keeps its completion value: `files[].result`, `Execute` and
  `runScript` report exactly what they did.

## Consequences

Cost, as callgrind instruction counts of the production `GocciaRunner`:

| Workload | Base | After | Change |
| --- | ---: | ---: | ---: |
| imported module: `for (const i of big) { total += b.byteLength; }`, 200,000 iterations | 7,645.90 M | 7,645.90 M | +3,305 instructions |
| the same loop, sandbox entry (`Execute`) | 6,600.04 M | 6,600.05 M | +3,941 instructions |
| 2,000 top-level `total += b.byteLength;` statements, sandbox entry | 391.86 M | 393.49 M | +0.42% |
| the same 2,000 statements, imported module | 413.16 M | 412.91 M | -0.06% |

A top-level loop pays nothing per iteration. Straight-line top-level code pays
about 800 instructions per statement, compiling included, and only once, since
a module's body runs once. An imported module's expression statements no longer
copy their value into the completion register, which more than pays for the
clears.

- The bytecode format stays at v101: no opcode or operand changes meaning, and
  `.gbc` files compiled before this change load and run as before, keeping what
  they kept.
- A statement nested in a top-level compound statement that awaited still
  clears at its own end, and the top-level statement clears the same
  registers again where it ends: `if (flag) await f(a, b);` stores undefined
  twice per register it used, once, since the body runs once. Skipping the
  inner clear only where nothing runs between the two would need the
  compiler to track which nested statements end their top-level statement,
  through blocks, branches, `try` and `finally`, for a few stores.
- A script's top level is unchanged. A global-backed script (a sandbox script
  entry) reads its bindings by name too, so the same copy stays there until the
  register is reused.
- A module entry still keeps its last expression statement's value, as its
  reported completion value: a bare `await make();` in an entry keeps the
  awaited value until the next expression statement replaces it.
- `GocciaBundler` compiles a module without knowing whether it will be an entry,
  so a bundled `.gbc` module keeps its completion value, also when imported.
