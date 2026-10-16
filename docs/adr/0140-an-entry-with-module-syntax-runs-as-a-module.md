# 0140 - An entry with a top-level import or export runs as a Module unless a source type is named

**Date:** 2026-10-14
**Area:** `modules`, `source pipeline`, `engine`, `CLI`
**Related:** [ADR 0058](0058-centralized-parser-policy.md), [ADR 0048](0048-opt-in-non-strict-compatibility.md), [ADR 0136](0136-module-function-declarations-are-compiled-while-linking.md)

## Context

An entry file runs with one of two goal symbols: Script (ES2026 §16.1) or
Module (§16.2). A `.mjs` or `.mts` entry, `--source-type=module`, a
`"source-type": "module"` config key or `Engine.SourceType := stModule` picks
Module. Any other entry ran as a Script, even with `import` and `export`
declarations, which GocciaScript has always accepted in Script source.

A Script entry has no Module Record. When another module imports the entry's
file, the loader loads that file a second time, as a module of its own. #1484
kept that, and documented it: a self-import or an import cycle through a
Script entry evaluates a second copy of the entry. In a cycle the second copy
runs while the first module it imports is still evaluating, so it reads that
module's bindings in their temporal dead zone
([#1626](https://github.com/frostney/GocciaScript/issues/1626)):

```js
// cyc.js (entry)
import { y } from "./d.js";
export const h = () => "h";
console.log(y); // ReferenceError: Cannot access 'y' before initialization

// d.js
import { h } from "./cyc.js";
export const y = "y";
```

Node.js prints `y`. Since v22.7.0 and v20.19.0 it detects the goal of an
ambiguous `.js` file (no `"type"` in its `package.json`) from its syntax, and
runs one with ES module syntax as an ES module. Applications split into
modules commonly have their entry in such a cycle (`main.js` and `state.js`).

## Decision

- **Without a named source type, the entry's syntax picks its goal.** An
  entry with a top-level `import` declaration or `export` declaration runs
  as a Module, and any other entry runs as a Script, as before. This reverses
  #1484 for entries that have module syntax: the entry is one Module Record,
  so an import of its file gets that record, and each module of a cycle
  evaluates once (§16.2.1.6.1.3.1 InnerModuleEvaluation, step 3).
- **Detection is syntactic, and the one parse does it.** The parser notes
  whether a top-level statement starts an import or export declaration, with
  the same dispatch it uses to parse one (`import(` and `import.` begin an
  expression). A dynamic `import()` or an `import.meta` reference does not
  count: both are expressions, and `import()` is valid Script code. The two
  errors that differ by goal are held until the parse ends: `import.meta`
  (a SyntaxError in a Script, §13.3.12) and a `with` statement (a SyntaxError
  in a Module, which is strict, §11.2.2). The settled goal then raises the one
  that applies. A Module's other early errors are checked after the parse, as
  for any Module. Only with `--compat-non-strict-mode` does a detected Module
  parse twice: the first parse was sloppy Script code, and a Module is not.
- **`stDetect` is the engine's default source type.** `TGocciaSourceType` has
  a third value. `TGocciaEngine.SourceType` defaults to it unless the entry
  is `.mjs` or `.mts`. An embedder that names `stScript` or `stModule` gets
  that goal whatever the source contains, so `stScript` restores the previous
  behaviour. `TGocciaSourcePipelineResult.SourceType` reports the goal a parse
  settled. `Execute` settles each run's goal without changing `SourceType`, so
  a long-lived engine settles each source on its own. A host that parses the
  entry itself assigns the settled goal to `SourceType` before
  `CompileModule` and `RunModuleForSourceType`.
- **An explicit Script keeps accepting module syntax.** `--source-type=script`
  and `"source-type": "script"` run the entry as a Script, with its imports and
  exports, exactly as before this decision. A SyntaxError for module syntax
  in an explicit Script would follow ECMA-262 more closely, but it would take
  away the one way to keep an entry's Script semantics (global `var`s, top-level
  `this`, the script-only compatibility flags) while it keeps its imports.
- **A Module's imported names are early errors.** A module that declares a
  name one of its imports binds, or two imports binding one name, is a
  SyntaxError before it runs (§16.2.1.1, with LexicallyDeclaredNames §8.2.4).
  The engine accepted it for every Module. A detected entry would have made
  more entries Modules that accept it silently, so the check joined the source
  pipeline's other Module early errors.

## Where it applies

| Entry point | Goal without a named source type |
|---|---|
| `GocciaRunner` file, directory and stdin entries | Detected |
| `GocciaRunner` sandbox entries (`--sandbox`, `runScript`) | Detected |
| `GocciaTestRunner` test files | Detected |
| `GocciaBenchmarkRunner` benchmark files and stdin | Detected |
| `GocciaBundler` | Detected; the settled goal picks strict or sloppy compilation |
| `GocciaScriptLoaderBare` and `GocciaWasmTestRunner` | Detected |
| `GocciaREPL` | Script: an input line is not an entry file, and the lines share one global environment, which only a Script has |
| `.gbc` entries | Script, as before: there is no source to detect from, so a module `.gbc` still needs `--source-type=module` |
| `GocciaTest262Runner` | Unchanged: each test names its goal from its flags |

Node.js also counts `import.meta`, top-level `await` and a lexical
redeclaration of a CommonJS wrapper name as module syntax. GocciaScript has no
CommonJS wrapper, allows top-level `await` in Script source, and keeps
`import.meta` an error in a Script, so only import and export declarations
decide. Node.js detects a file whose nearest `package.json` has no `"type"`;
GocciaScript has no package-level type and uses `goccia.json`'s
`"source-type"` instead. A type-only `import type` or `export type`
declaration counts, as it does for TypeScript; Node.js v24.21.0 runs a `.ts`
file whose only module syntax is `export type` as CommonJS.

## Consequences

- An entry with a top-level import or export behaves as an imported module
  does: its top-level `var` and function declarations are not global object
  properties, top-level `this` is `undefined`, `import.meta` is available, and
  the code is strict.
- The script-only compatibility behaviour stops applying to such an entry:
  `--compat-non-strict-mode` (sloppy assignment, `delete`, `with`, `this`
  coercion), the mapped `arguments` object, Annex B block functions, and the
  global-object `var` binding of `--compat-var`. Name `--source-type=script`
  to keep them.
- In this repository's `tests/` tree, 71 files outside a
  `"source-type": "module"` directory have a top-level import or export, so
  the test runner now runs them as modules. The suite passes without changes
  to them.
- Detection costs one check per top-level statement of the parse it
  already does. Under callgrind, with a production build, a 228 KB Script
  entry of 3,001 top-level statements ran 8,856,448,966 instructions, against
  8,856,019,736 before (+0.005%), and the same file with one `export` added
  ran 12,464,797,818 detected against 12,464,180,646 as an explicit Module
  before.
