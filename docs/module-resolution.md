# Module Resolution

*How an import specifier becomes a file: the resolution order, the opt-in `node_modules` lookup, and where GocciaScript deliberately differs from Node.*

## Executive Summary

- **Four kinds of specifier** — virtual modules, aliases and import maps, relative and absolute paths, and (opt-in) bare package names, tried in that order
- **Bare specifiers are sealed by default** — `import "zod"` fails until the engine's `import` capability grants `node_modules` (`--allow-import=node_modules` on the command line), keeping the default profile free of ambient package lookup
- **A subset of Node's ESM resolver** — the `exports` map with the `import` and `default` conditions, wildcard patterns, and the legacy entry fields; no `require`/`node` conditions, no `imports` map, no self-reference
- **Two deliberate deviations** — the bundler-only `module` field is honoured (Node ignores it), and a package that resolves to CommonJS is refused by name instead of being parsed
- **The sandbox stays sealed** — `GocciaRunner`'s sandbox mode offers no `node_modules` opt-in, because its filesystem holds only copied inputs rather than a tree to walk

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
   [Build System](build-system.md#compile-and-run). An entry whose address is
   `github:<owner>/<repo>@<ref>[/<path>]` names a lockfile-pinned provider
   package instead of a path; see [Provider Imports](provider-imports.md).
3. **Relative and absolute paths** — `./`, `../`, and host-absolute
   specifiers, resolved against the importing file's directory.
4. **Bare specifiers** — a package name, resolved against `node_modules` only
   when the capability below has been granted.

Steps 2–4 all end in the same extension probe: the exact path first, then the
TypeScript source candidates for a `.js`-style specifier, then each configured
extension, then `<path>/index.<ext>`. A specifier therefore does not need a
file extension, and a directory resolves to its `index` file.

Within a realm, an on-disk module is loaded once per canonical file path and
import type. A symlink or junction, a different spelling of the same name on
a case-insensitive filesystem, and a specifier resolved through an import map
or package can all reach the same module record. This also applies to the
entry module. Relative imports in a file resolve from its canonical directory.
Diagnostics, coverage, and `import.meta.url` use the path the module was first
loaded under. `import.meta.resolve` computes its result lexically. The loader
can reload an on-disk source module when it detects a newer version.

Read permissions are checked for each import before the module cache is
consulted. The check follows symlinks to the file being imported.

## Enabling `node_modules`

```bash
# Walk up from each importing file, exactly as Node does.
./build/GocciaRunner app.js --allow-import=node_modules

# Same walk, but no node_modules above ./project is ever consulted.
./build/GocciaRunner app.js --allow-import=node_modules=./project

# GocciaTestRunner, GocciaBenchmarkRunner and GocciaREPL take it too
# (GocciaBundler grants no capabilities), and so does a trusted config file.
./build/GocciaTestRunner app.test.js --allow-import=node_modules
```

```json
{
  "permissions": {
    "allow-import": ["node_modules=."]
  }
}
```

A config's `permissions` block is a request: it applies only once the config
is trusted (`GocciaRunner --trust <path>`), or for one run with `-P` /
`--accept-config-permissions`. See [Config trust](permissions.md#config-trust).

Without the grant, a bare specifier fails with `Cannot resolve bare module
specifier "<name>". Imports must start with "./" or "../"`. This is a
capability in the sense [VISION](../VISION.md) uses the word: the engine gains
the ability to read a directory tree the script never named, so a host has to
ask for it. The grant is the engine's `import` capability for
`node_modules` — `node_modules=<dir>` when it names a ceiling — and the
resolver asks the engine's set before every walk; an embedder grants the same
scope with `TGocciaCapabilities.Allow(gcImport, 'node_modules')`. See
[Permissions](permissions.md#import-scopes) for how grants and denies combine;
an explicit deny throws `PermissionDenied` instead of the sealed message.

A package the walk reaches through a granted `node_modules` scope is part of
the [module graph](permissions.md#the-module-graph-exemption): its files need
no `read` grant, wherever that `node_modules` directory is, though a `read`
deny covering them still refuses them.

The optional value is a **ceiling**, not a starting point. The walk still
begins at the importing file's directory, so a package that ships its own
nested `node_modules` still resolves; the ceiling only stops the walk from
climbing past the directory named. Together with the package-boundary rules
below — which keep a resolved file inside the package it was found in — that is
what makes the option safe to point at a project root: nothing outside it can
satisfy an import.

A ceiling grants the walk only to importers inside it; a bare specifier
imported from outside every granted ceiling stays sealed.

A relative ceiling is anchored to whatever supplied it: the invocation
directory for the command-line flag, and the configuration file's own directory
for a `permissions` entry, the same rule relative `--alias` targets follow. The ceiling
is compared against expanded host paths, so give it the path the engine will
see; a spelling that reaches the same directory through a symlink does not
match, and the resolver fails closed rather than widening the boundary.

In a config file the two forms are the scopes `"node_modules"` (an unbounded
walk) and `"node_modules=<dir>"` (a ceiling). `import` always takes a scope, so
`"allow-import": true` and a bare `--allow-import` are errors.

Sandbox mode has no equivalent: `--allow-import` is a usage error there. Its
filesystem is an in-memory image of the copied inputs with no ambient host
access, so there is nothing to walk up into; a bare specifier there fails with
the same message and no opt-in exists.

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
6. **Refuse anything outside the package.** See below.
7. **Refuse CommonJS.** A resolved file that is CommonJS raises a named error
   rather than reaching the parser.

### The package boundary

A resolved file must live inside the package directory it was found in. Three
checks enforce it, and a failure of any is an ordinary `Module not found:
"<specifier>"` refusal:

- **Segment validation.** A subpath, a pattern's star value, or an `exports`
  target whose segments include `.`, `..`, or `node_modules` is rejected —
  Node's `invalidSegmentRegEx`. This is what stops
  `import "pkg/../../secrets.js"` on the legacy path and
  `import "pkg/sub/../../../secrets"` through a `"./sub/*"` pattern, where the
  star value is the half of the substitution the importer controls.
- **Target shape.** Every `exports` string target must start with `./`, as
  Node requires; `"exports": "../../outside.js"` is a malformed package rather
  than a way out of one.
- **Containment.** The final expanded candidate is checked against the package
  directory before the extension probe and again after it. Segment validation
  rejects what is invalid on its face; this catches whatever any combination
  still normalized into. The pre-probe check compares normalized spellings,
  because the candidate is a name that need not exist yet. The post-probe check
  is **physical**: by then a real file has been found, and both it and the
  package directory are canonicalized — every symbolic link on either path
  resolved — before the comparison. A package that ships
  `linked/out.js -> ../../../outside.js` normalizes to a path inside itself
  while naming a file outside it, and only the physical check refuses that.
  Under the default capabilities such a link is usually refused earlier, by
  the `read` check, as `PermissionDenied: read: <specifier>`; the physical
  check is what still refuses it when a `read` grant covers the target. It
  follows the same principle as [ADR 0071](adr/0071-reject-symlinks-in-sandbox-seed-imports.md),
  where the sandbox refuses a symlinked copy input rather than trusting where
  its name appears to sit.

  Canonicalizing the package directory as well as the candidate is what keeps
  pnpm-style layouts working. There `node_modules/<pkg>` is itself a link into
  a content-addressed store, so resolving only the candidate would place every
  file in every pnpm package outside its own root; resolving both moves the
  comparison into the store, where a store-internal file passes and a link that
  leaves the store still does not.

  **Platform support.** Canonicalization uses `realpath(3)` on Linux, macOS and
  the BSDs, and `GetFinalPathNameByHandleW` on Windows, which follows both
  symbolic links and directory junctions. Where a host cannot canonicalize a
  path — the Lakon/WASI lane, whose in-memory filesystem has no links at all,
  or a Windows file the process cannot open even for metadata — the normalized
  spelling comparison stands on its own and the boundary is lexical for that
  resolution.

The legacy no-`exports` path is stricter here than Node, which would let
`new URL(subpath, packageURL)` walk upward. A subpath containing `..` is never
a legitimate import, and the ceiling above is only a real boundary if the
package boundary holds too.

### The `exports` map

The supported shapes, all of which are what packages in practice use:

| Shape | Behavior |
|---|---|
| `"exports": "./index.js"` | Main entry only; any subpath is not exported |
| `"exports": { "import": "…", "default": "…" }` | Conditions for the main entry |
| `"exports": { ".": …, "./sub": … }` | Subpath map, exact keys |
| `"exports": { "./x/*": "./src/*.ts" }` | Wildcard pattern; every `*` in the target is replaced with the matched text |
| A nested condition object | Recursed into, to any depth |
| An array of targets | First valid target wins; a well-formed target whose file is missing does not fall through to the next |
| `null` | The subpath is deliberately not exported |

**Conditions.** Only `import` and `default` are understood. Node's default ESM
condition set is `["node", "import"]`; `node` is deliberately absent because
GocciaScript is not a Node host and a `node` branch usually leads to CommonJS
or to `node:` built-ins that do not exist here (a runtime extension registers only
a few, such as `node:async_hooks`). A condition key that is not
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
  markers (an `import` or `export` keyword followed by whitespace, `{`, `*`,
  or a quote) is CommonJS.

The source scan is a heuristic, and it is asymmetric on purpose. A file with
both shapes — an interop shim calling `require` from an ES module — is read as
an ES module, which is what every other toolchain does with it. A file with
neither is inert and loads either way. Reading the file text rather than
trusting `"type"` is what makes the `module`-field deviation above work at all,
since those ES module builds routinely sit in packages that declare no type.

Before the markers are looked for, a lexical pass replaces every comment,
string body, template body, and regular expression literal with a placeholder,
so a keyword the file only mentions counts for nothing. Every esbuild
`__toCommonJS` bundle depends on that pass: it ends with the banner comment
`// Annotate the CommonJS export names for ESM import in node:`, whose bare
`export` and `import` words used to carry the whole file past the check. Such a
bundle was loaded and then failed at its first `require`, with a
`require is not defined` reference error instead of the package-relative
message below.

The placeholder is chosen for what stood there. A string, template, or regular
expression is an operand, so it collapses to a value; a comment is not, so it
collapses to a space the scan looks past. That distinction is what keeps
`var a = "p" / 2` dividing rather than opening a literal at the slash.

The pass is lexical, not a parse, and it decides one thing by approximation:
whether a `/` opens a regular expression or divides, read from the last
significant character before it. Division needs a value in front, so an
identifier — including a non-ASCII one such as `café` — a number, a closing
bracket, a postfix `++`, or a collapsed literal all mean division, and every
other punctuator leaves an operand position where a slash opens a literal. Two
shapes still come out wrong:

- A literal opening straight after a block's closing brace or a condition's
  `)`, as in `if (ok) /re/.test(x)`, is read as division, and its body is then
  scanned as ordinary code. A quote inside the body can open a string scan that
  runs to the next quote on the line.
- A division after an identifier that spells one of the keywords a literal may
  follow, as in `const of = 4; of / 2`, is read as a literal.

The second is the damaging one. If a second `/` follows on the line, the bogus
literal closes and everything between the two is discarded along with any
marker in it — enough, in principle, to turn a genuine ES module into a
CommonJS refusal. Only when no second `/` follows is the slash kept as code and
nothing lost. A source the pass cannot finish at all — an unterminated block
comment or template literal — is classified on its raw text instead, which
restores the pre-pass behaviour for that one file.

What survives the pass is a marker the file builds at runtime, `["exp" + "ort"]`
or a keyword it only ever names in data. That remains a false *negative*, and it
is the deliberate direction of the asymmetry: the file is loaded rather than
refused, and removing the last of them would cost a parse of every resolved
package entry.

A file classified as CommonJS raises:

```text
Package "async-channel" resolved to a CommonJS file (index.js); GocciaScript loads only ES modules
```

The path in the message is **package-relative**. Per
[ADR 0108](adr/0108-specifier-only-module-resolution-errors.md) no expanded
host path may reach script-visible error text; the package-relative spelling
names the file without disclosing where the package lives. Host reporters still
print the absolute path on the trailing `Resolved to:` line, because the engine
keeps the candidate path when it turns the failure into a script error
(`TGocciaRunResult.ErrorResolvedPath`). The typed `EModuleIsCommonJS` exception is
only visible to a host calling `TModuleResolver.Resolve` directly: the engine
turns every resolution failure into a plain `Error` carrying its message, so a
host going through `TGocciaEngine` distinguishes this case by its message, not by
its class.

The error is a module-resolution failure, so it is catchable from script
through `import()` and it is never a `SyntaxError` — the CommonJS source is
never handed to the parser.

## Not implemented

The following parts of Node's resolver are absent. Each fails as a plain
resolution error rather than misbehaving:

- The `imports` map and `#`-prefixed private specifiers
- Package self-reference by the manifest's own `name`
- The `require`, `node`, `browser`, `types`, and custom conditions
- `node:` built-in modules other than those a runtime extension registers (such as `node:async_hooks`), and Node host APIs other than the subsets that runtime extensions provide (such as the sandbox `fs` module)
- `package.json` `"type"` inheritance for files outside a package

Loading CommonJS itself does not ship as part of GocciaScript, because userland
can shim it: convert the package to ESM with a bundler (`esbuild --bundle --format=esm`,
or Rollup with `@rollup/plugin-commonjs`) and run the output with the
compatibility flags, since the default profile rejects bundler output. For a
CommonJS entry point esbuild emits only a default export (`export default
require_x()`), so a consumer that uses named imports needs a small ESM wrapper
that re-exports the names it wants from that default. The converted output is
module code, which is always strict, so a package that relies on non-strict
behaviour (such as mapped `arguments`) can behave differently after
conversion. esbuild targets the browser platform by default, so a dependency
can resolve to a browser build that expects a DOM; choose the bundler's
platform and conditions to suit the host (the bundler's own documentation
covers these options). A package that needs a Node built-in is not solved that
way; see [VISION](../VISION.md#what-gocciascript-is-not).

## Runtime code frames

When an uncaught error terminates a run, the diagnostic can show a code frame —
the source line the error came from, with a caret. Where that line is comes from
the engine's own record of where the error was created, never from the thrown
value's `stack` string:

- **Provenance, not `stack`.** When the engine creates an error it keeps the
  stack it made the error with in an internal slot the script cannot see or
  change (`ErrorStackProvenance` in `Goccia.VM`), and `TGocciaEngine.Run` reports that
  stack's first frame as the error's file, line and column. A guest can
  overwrite an error's `.stack` property, or `throw` a plain object with a
  fabricated `stack` — that string is never parsed to pick a file or read one,
  so a forged frame discloses nothing and no host opens a guest-named path. A
  thrown value with no engine-recorded provenance gets no code frame (its
  message still shows).

- **A guest never reads host source.** The excerpt is read when the error is
  rendered, not carried on the error. `FailureText` (`Goccia.CLI.FileEngine`)
  shows the entry's own source, and another file's only when
  `TGocciaEngine.ModuleLoaded` says the run loaded it as a guest module. The
  modules the host loads before the entry runs (`--globals`,
  `--host-environment`, `--module`, `--modules` and their config-file and
  embedding forms) are host-owned, and so is every module they import, even
  when guest code calls an exported host function later (`HostOwnedReferrer`
  in `Goccia.Modules`). A genuine error thrown inside a host-owned module shows its
  location line but **no source excerpt**.

- **Canonical file identity.** A guest import of a file the host already loaded
  under another name — through a symlink, a hardlink or a case alias — is
  matched to the host's module by canonical identity (`CanonicalHostPath`:
  `realpath` on POSIX, an open handle on Windows), so it keeps the host's
  ownership instead of exposing its source. When the identity cannot be
  obtained the file is not matched, and only a module the guest's run itself
  loaded is shown.

- **Read from where the module came from.** The host reads the excerpt from
  the file on disk, or in sandbox mode from the snapshot of the virtual
  filesystem the module was loaded from. A file it cannot read gives the
  location line without an excerpt.

The bytecode side is described in
[Bytecode VM — Runtime Error Diagnostics](bytecode-vm.md#runtime-error-diagnostics).

## Related documents

- [Build System](build-system.md) — the authoritative CLI and config reference
- [Virtual Module Configuration](virtual-modules.md) — host-supplied modules, which resolve first
- [Provider Imports](provider-imports.md) — `github:` import-map entries, the lockfile, and the cache
- [Language](language.md#modules) — module syntax and the supported file extensions
- [Embedding](embedding.md) — `AddAlias` and resolver configuration from a host
- [ADR 0108](adr/0108-specifier-only-module-resolution-errors.md) — why resolution messages carry no host path
