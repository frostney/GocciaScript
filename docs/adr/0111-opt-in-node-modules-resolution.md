# Opt-in node_modules resolution

**Date:** 2026-08-20
**Area:** `modules`, `security`, `cli`
**Related:** [ADR 0103](0103-layered-untrusted-execution-boundaries.md), [ADR 0108](0108-specifier-only-module-resolution-errors.md)

## Context

`import "zod"` failed with `Cannot resolve bare module specifier "zod"`. The
only way to load an npm package was to name every one of its files by hand:
`--alias zod=./node_modules/zod/index.js`, repeated for each transitive
dependency, since the packages a package imports are themselves bare. A real
chain — `zod` -> `tldts` -> `tldts-core` — needed three aliases that a user had
to discover by reading each `package.json`, and the aliases broke whenever a
dependency's entry point moved.

The refusal was not an accident. [VISION](../../VISION.md) states that
GocciaScript does not target Node host compatibility, and the sandbox-first
posture means the engine should not read directory trees the script never
named. Resolving `node_modules` by default would give every script an ambient
lookup path reaching to the filesystem root.

So the question was not whether bare specifiers *can* resolve, but whether the
capability can be granted deliberately, and how much of Node's resolver has to
come with it.

## Decision

Bare-specifier resolution against `node_modules` exists, is off by default, and
is a subset of Node's ESM resolver.

- **Capability, not a default.** `--allow-node-modules` (and the matching
  config key) grants it on the shared CLI hosts. Without it the pre-existing
  refusal message is unchanged, so the default profile gains no ambient
  authority. An optional value — `--allow-node-modules=<dir>` — is a *ceiling*:
  the walk still starts at the importing file, so nested `node_modules` still
  resolve, but nothing above `<dir>` is ever probed.
- **`TGocciaSandboxModuleResolver` stays sealed.** Its filesystem is a seeded
  in-memory image with no ambient host access, so there is no ancestor tree to
  walk and no opt-in is offered. The two resolvers share the refusal message
  constant so they cannot drift apart in wording.
- **The supported surface is what packages actually ship.** The `exports` map
  with string targets, condition objects, arrays, `null` blocks, and `*`
  patterns ranked by Node's `PACKAGE_EXPORTS_RESOLVE` specificity; then the
  legacy entry fields. The `imports` map, self-reference, and `node:` builtins
  are out.
- **Only the `import` and `default` conditions are selected.** Node's default
  ESM condition set is `["node", "import"]`. `node` is dropped deliberately: a
  `node` branch leads to CommonJS or to Node built-ins, neither of which this
  engine has.
- **The `module` field is preferred over `main`, which Node does not do.**
  Node ignores `module` entirely. GocciaScript loads only ES modules, and the
  usual shape of an `exports`-less package is a CommonJS `main` beside an ES
  module `module`. Reading `main` first would refuse packages that ship a
  usable ES module build. `exports`, when present, still short-circuits both.
- **CommonJS is refused by name, not by parse failure.** A resolved file is
  classified by extension (`.mjs`/`.mts` and `.cjs` are decisive), then by the
  manifest's `"type"`, then by scanning the source for CommonJS markers with no
  ES module markers. A CommonJS file raises `EModuleIsCommonJS` — a subclass of
  the resolver's not-found exception, so it flows through the existing
  rewrapping path and stays catchable from `import()`.
- **The refusal message obeys ADR 0108.** It names the file
  *package-relatively* (`index.js`), never by expanded host path; the absolute
  path travels in `ResolvedCandidatePath` to host reporters only.

## Consequences

An npm dependency chain now loads with one flag and no aliases, which is what
makes third-party ES module packages usable at all. The flag is the whole
boundary: anything that can pass it can also pass `--alias`, so this grants no
authority a host did not already have, but it does widen what a *single*
grant reaches — every ancestor `node_modules` rather than one named file. The
ceiling form exists for hosts that want the narrower grant.

The `module`-field preference means GocciaScript and Node resolve the same
`exports`-less package to different files. That is a real divergence, not a
bug, and it is the reason a differential suite can only gate the shapes both
runtimes agree on: `scripts/differential/m-nodemods.test.js` is bun-gated, and
the two goccia-only behaviours live in `n-nodemods.goccia.test.js`.

The CommonJS classifier reads module source at resolution time and decides by
heuristic. A file with both `require` and `export` is read as an ES module,
which is right for interop shims and wrong for nothing seen so far; a file that
declares `"type": "module"` is never scanned. The cost is one extra read per
`node_modules` resolution, paid only when the capability is on.

None of this is a step toward Node host compatibility. CommonJS is refused
rather than deferred, `node:` specifiers stay unresolvable, and the resolver
reads five fields out of `package.json`.
