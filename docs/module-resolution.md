# Module Resolution

*How an import specifier becomes a file: the resolution order, the opt-in `node_modules` lookup, and where GocciaScript deliberately differs from Node.*

## Executive Summary

- **Four kinds of specifier** — virtual modules, aliases and import maps, relative and absolute paths, and (opt-in) bare package names, tried in that order
- **Bare specifiers are sealed by default** — `import "zod"` fails until a host passes `--allow-node-modules`, keeping the default profile free of ambient package lookup
- **A subset of Node's ESM resolver** — the `exports` map with the `import` and `default` conditions, wildcard patterns, and the legacy entry fields; no `require`/`node` conditions, no `imports` map, no self-reference
- **Two deliberate deviations** — the bundler-only `module` field is honoured (Node ignores it), and a package that resolves to CommonJS is refused by name instead of being parsed
- **The sandbox host stays sealed** — `GocciaSandboxRunner` offers no `node_modules` opt-in, because its filesystem is seeded by the embedder rather than walked

## Resolution order

`TModuleResolver.Resolve` (`source/units/Goccia.ModuleResolver.pas`) is asked
for a specifier and the path of the file that imported it, and answers with a
host path. The module loader tries the steps in this order and stops at the
first that answers:

1. **Virtual modules** — host-supplied modules registered through `--module`,
   `--modules`, or an embedding API, including the bundled `vitest`
   compatibility shim. See [Virtual Module Configuration](virtual-modules.md).
2. **Aliases and import maps** — `--alias key=value`, `--import-map=<file>`,
   and the `imports` object of a discovered `goccia.json`. Longest matching
   prefix wins. Commands and precedence live in
   [Build System](build-system.md#compile-and-run).
3. **Relative and absolute paths** — `./`, `../`, and host-absolute
   specifiers, resolved against the importing file's directory.
4. **Bare specifiers** — a package name, resolved against `node_modules` only
   when the capability below has been granted.

Steps 2–4 all end in the same extension probe: the exact path first, then the
TypeScript source candidates for a `.js`-style specifier, then each configured
extension, then `<path>/index.<ext>`. A specifier therefore does not need a
file extension, and a directory resolves to its `index` file.

## Enabling `node_modules`

```bash
# Walk up from each importing file, exactly as Node does.
./build/GocciaScriptLoader app.js --allow-node-modules

# Same walk, but no node_modules above ./project is ever consulted.
./build/GocciaScriptLoader app.js --allow-node-modules=./project

# Any shared CLI host takes it, and so does a config file.
./build/GocciaTestRunner tests --allow-node-modules
```

```json
{
  "allow-node-modules": true
}
```

```json
{
  "allow-node-modules": "./project"
}
```

Without the option, a bare specifier fails with `Cannot resolve bare module
specifier "<name>". Imports must start with "./" or "../"` — the pre-existing
behavior, unchanged. This is a capability in the sense
[VISION](../VISION.md) uses the word: the engine gains the ability to read a
directory tree the script never named, so a host has to ask for it.

The optional value is a **ceiling**, not a starting point. The walk still
begins at the importing file's directory, so a package that ships its own
nested `node_modules` still resolves; the ceiling only stops the walk from
climbing past the directory named. That is what makes the option safe to point
at a project root: nothing outside it can satisfy an import.

The value is also how a config file distinguishes the two forms. `true` (or a
bare `--allow-node-modules`) means an unbounded walk, `false` means the option
is off, and any other string is the ceiling directory.

`GocciaSandboxRunner` has no equivalent. Its filesystem is a seeded in-memory
image with no ambient host access, so there is nothing to walk up into; a bare
specifier there fails with the same message and no opt-in exists.

## The algorithm

Given a bare specifier and the importing file:

1. **Split.** The package name is the first path segment, or the first two when
   the specifier starts with `@` (`@convex-dev/workpool/test` is the package
   `@convex-dev/workpool` and the subpath `./test`). A specifier with no
   subpath looks up the `.` key. `#private` imports, `node:`-style protocol
   specifiers, and a bare `@scope` are not package specifiers and are refused.
2. **Walk.** Starting at the importing file's directory and moving upward,
   probe `<dir>/node_modules/<package>` until one exists or the ceiling (or
   the filesystem root) is reached. A directory already named `node_modules` is
   skipped rather than being given a `node_modules/node_modules` child, which
   is what Node's `NODE_MODULES_PATHS` does.
3. **Read the manifest.** `<package>/package.json` supplies `name`, `type`,
   `main`, `module`, and `exports`. Nothing else is read.
4. **Select a target.** `exports` decides when present; otherwise the legacy
   fields do. Both are described below.
5. **Probe extensions.** The selected target is resolved through the same
   extension probe every other specifier uses, so `"exports": {"./x/*":
   "./src/*.ts"}` reaches a real `.ts` source and a target with no extension
   still finds one.
6. **Refuse CommonJS.** A resolved file that is CommonJS raises a named error
   rather than reaching the parser.

### The `exports` map

The supported shapes, all of which are what packages in practice use:

| Shape | Behavior |
|---|---|
| `"exports": "./index.js"` | Main entry only; any subpath is not exported |
| `"exports": { "import": "…", "default": "…" }` | Conditions for the main entry |
| `"exports": { ".": …, "./sub": … }` | Subpath map, exact keys |
| `"exports": { "./x/*": "./src/*.ts" }` | Wildcard pattern; every `*` in the target is replaced with the matched text |
| A nested condition object | Recursed into, to any depth |
| An array of targets | First entry that resolves wins |
| `null` | The subpath is deliberately not exported |

**Conditions.** Only `import` and `default` are understood. Node's default ESM
condition set is `["node", "import"]`; `node` is deliberately absent because
GocciaScript is not a Node host and a `node` branch usually leads to CommonJS
or to `node:` built-ins that do not exist here. A condition key that is not
`import` or `default` — `require`, `browser`, `types`, `node` — is skipped, and
resolution continues with the next key. The remaining keys are tried in the
order the manifest writes them, as Node's `PACKAGE_TARGET_RESOLVE` does, so a
`default` written before `import` really does win. Packages write `default`
last for that reason.

**Specificity.** An exact key always beats a pattern. Among patterns, the one
with the longest text before its `*` wins, and the longest text after the `*`
breaks a tie. This is Node's `PACKAGE_EXPORTS_RESOLVE` ordering.

**Refusal.** When a package has an `exports` map, the map is the whole surface:
an unlisted subpath, or one whose target is `null`, fails with `Module not
found: "<specifier>"` and never falls back to the legacy fields.

### Without an `exports` map

The main entry is `module`, then `main`, then `./index` through the extension
probe. A subpath is taken literally and joined to the package directory.

**Preferring `module` over `main` is a deliberate deviation.** Node ignores the
`module` field entirely; it is a bundler convention. GocciaScript loads only ES
modules, and the common shape for a package with no `exports` map is a
CommonJS `main` beside an ES module `module` — Node reads the former because it
can, and this engine reads the latter because it is the only one it can use.
Preferring `main` would mean refusing a package that ships a perfectly good ES
module build. A package with an `exports` map is unaffected, because `exports`
short-circuits both fields.

## CommonJS is refused, not parsed

A resolved file inside a package is classified before it is loaded:

- `.mjs` and `.mts` are ES modules, and `.cjs` is CommonJS, both regardless of
  content.
- A package whose manifest declares `"type": "module"` ships ES modules.
- Otherwise the source text decides: a file carrying CommonJS markers
  (a `require(...)` call, `module.exports`, `exports.x`) and no ES module
  markers (a statement-position `import` or `export`) is CommonJS.

The source scan is a heuristic, and it is asymmetric on purpose. A file with
both shapes — an interop shim calling `require` from an ES module — is read as
an ES module, which is what every other toolchain does with it. A file with
neither is inert and loads either way. Reading the file text rather than
trusting `"type"` is what makes the `module`-field deviation above work at all,
since those ES module builds routinely sit in packages that declare no type.

A file classified as CommonJS raises:

```
Package "async-channel" resolved to a CommonJS file (index.js); GocciaScript loads only ES modules
```

The path in the message is **package-relative**. Per
[ADR 0108](adr/0108-specifier-only-module-resolution-errors.md) no expanded
host path may reach script-visible error text; the package-relative spelling
names the file without disclosing where the package lives. Host reporters still
print the absolute path on the trailing `Resolved to:` line, and an embedding
host can catch the typed `EModuleIsCommonJS` exception for both.

The error is a module-resolution failure, so it is catchable from script
through `import()` and it is never a `SyntaxError` — the CommonJS source is
never handed to the parser.

## Not implemented

The following parts of Node's resolver are absent. Each fails as a plain
resolution error rather than misbehaving:

- The `imports` map and `#`-prefixed private specifiers
- Package self-reference by the manifest's own `name`
- The `require`, `node`, `browser`, `types`, and custom conditions
- `node:` built-in modules, and any Node host API
- `package.json` `"type"` inheritance for files outside a package

Loading CommonJS itself is not on the roadmap; see
[VISION](../VISION.md#what-gocciascript-is-not).

## Related documents

- [Build System](build-system.md) — the authoritative CLI and config reference
- [Virtual Module Configuration](virtual-modules.md) — host-supplied modules, which resolve first
- [Language](language.md#modules) — module syntax and the supported file extensions
- [Embedding](embedding.md) — `AddAlias` and resolver configuration from a host
- [ADR 0108](adr/0108-specifier-only-module-resolution-errors.md) — why resolution messages carry no host path
