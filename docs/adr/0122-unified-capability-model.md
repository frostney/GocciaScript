# 0122 - One capability model: explicit allow/deny grants, trusted config, one runner

**Date:** 2026-09-25
**Status:** Accepted (implementation tracked in [#1255](https://github.com/frostney/GocciaScript/issues/1255))
**Area:** `runtime`, `host capabilities`, `cli`
**Supersedes:** the flag and default decisions in
[ADR 0068](0068-goccia-sandbox-runner.md),
[ADR 0103](0103-layered-untrusted-execution-boundaries.md) (host filesystem
loading on by default),
[ADR 0111](0111-opt-in-node-modules-resolution.md) (flag spelling),
[ADR 0119](0119-host-applied-sandbox-write-back.md) (flag spelling), the
event names in [ADR 0097](0097-engine-owned-capability-audit-seam.md), and the
`--remote-imports` flag proposed as ADR 0121 in #1054

## Context

Host authority grew one flag at a time, and each flag used its own grammar:
`--allowed-host`, `--fetch-deny-private-ranges`, `--unsafe-ffi`,
`--allow-node-modules[=ceiling]`, `--no-host-filesystem`, and the proposed
`--remote-imports`. Three problems followed.

- **The defaults contradicted [VISION](../../VISION.md).** VISION promises
  "no ambient file system", but host-filesystem module loading was on by
  default, so `import x from "/abs/path" with { type: "bytes" }` read any file
  the process could read.
- **Config granted itself authority.** A `goccia.json` discovered by walking
  up from each script could enable FFI, network hosts, `node_modules`, and the
  `unsafe-*` escapes. Running a cloned project could reach the network or load
  native code without anyone typing a flag, and a `git pull` could widen that
  silently.
- **Nothing was uniform.** Flag options ignored their value
  (`--unsafe-ffi=false` enabled FFI). There was no negation. Several binaries
  accepted capability options and ignored them. The embedding API spread
  grants across `AttachRuntime`, `SetAllowedFetchHosts`, the process-global
  `SetFetchRequestPolicy`, `AllowNodeModules`, and extension installs.

## Decision

**One engine-owned capability set.** Capabilities are an immutable set fixed
when the engine starts. The CLI, config files, and embedders only populate it.
Nested contexts (`runScript` children, ShadowRealm, workers) can narrow their
parent's set, never widen it. Fetch policy moves from process-global state to
the engine.

**Deny by default.** Without grants, nothing reaches outside the process. The
one exemption is the module graph: static imports with literal specifiers of
code and data modules inside the project. The project is the directory of the
nearest `goccia.json`, or the entry file's directory when there is none. There
are no interactive prompts; a missing capability is an error.

**One grammar.** `--allow-<cap>[=scope,…]` and `--deny-<cap>[=scope,…]`. A deny
always wins over an allow, regardless of order, so a deny can carve an
exception out of a broad allow. There is no `--allow-all` and no
environment-variable form of any grant.

| Capability | Scopes | Covers |
|---|---|---|
| `read` | canonical paths (symlinks resolved), recursive | host reads beyond the module-graph exemption, including dynamic `import()` with non-literal specifiers |
| `net` | `host`, `host:port`, `*.domain`, IP, CIDR, `private` | `fetch`; private and loopback ranges are denied unless named, and every redirect hop is re-checked |
| `ffi` | library paths | opening native libraries |
| `import` | `node_modules[=ceiling]`, provider hosts such as `github` | non-local module sources |

CLI scopes are relative to the working directory; config scopes are relative to
the config file.

**Settings stay settings.** Limits are not capabilities. They move to one
`--max-<resource>` family with unit suffixes: `--max-memory`,
`--max-fetch-bytes`, `--max-fs-bytes`, `--max-fs-nodes`, `--max-instructions`,
`--max-stack`. `--timeout` also accepts units. The `--unsafe-*` language
escapes, `--compat-*`, `--experimental-*`, and `--deterministic` remain
separate from the grammar.

**Config requests; the user grants.** A config's `permissions` block, its
`unsafe-*` keys, and its `sandbox` inputs are requests, not grants.

- `--trust <path>` shows each declaring config under that path and the change
  since it was last trusted. It then records the config's path and a SHA-256
  of its normalized effective block (after `extends` resolution) in a
  user-level store outside any repository. `--untrust` and `--list-trusted`
  manage the store.
- At startup, a config whose hash is not trusted fails the run and names the
  fix.
- `-P` trusts the declared blocks for one invocation. Automation and CI use it.
- Other config settings (`compat-*`, import maps, aliases, limits) still apply
  automatically.

**Binaries reject what they cannot honor.** A capability flag that a binary
does not support is an error. The same capability coming from config is a
warning, so one project config can serve several binaries.

**One runner with a sandbox mode.** `GocciaScriptLoader` becomes `GocciaRunner`
and absorbs `GocciaSandboxRunner`. The runner has two filesystem modes.

- **Host mode** (the default) gives the module graph plus any granted
  capabilities.
- **Sandbox mode** gives a virtual filesystem, the `fs` and `goccia` (`$`,
  `runScript`) modules, diffs, and write-back.

Sandbox mode is switched on by:

- `--copy <host>[=<sandbox>]`, which copies an input in read-only;
- `--copy-rw`, which also marks the input for write-back under ADR 0119's
  rules;
- a trusted `sandbox` config section;
- `--sandbox`.

The runner's other sandbox options:

- The positional entry is a host file, copied into the sandbox automatically.
  `--entry` names a sandbox-only entry.
- `--diff[=json|unified]` prints the diff. `--diff-file=<path>` writes it
  instead, inferring the format from the file extension.
- Only `net` is honored in sandbox mode.

**Denials are visible and auditable.** A denied operation throws a catchable
`PermissionDenied`. Its message names the capability and the requested scope,
never host paths ([ADR 0108](0108-specifier-only-module-resolution-errors.md)).
The host-side report adds the flag or `--trust` command that would grant it.
Audit event kinds are named after capabilities (`net.fetch`, `read.file`,
`ffi.open`, `import.node-modules`, `import.provider`), and every allow and deny
decision emits one. Two further events are added: `config.permissions` for
trust decisions and `capabilities.effective` for the final set.

**Provider imports use `import`.** `--allow-import=github` materializes
lockfile-pinned, hash-verified provider packages when imports are resolved.
There are no default provider hosts, and the materialized files belong to the
module graph.

**One breaking release.** Old flags and config keys fail with an error naming
their replacement, with no aliases, following ADRs 0046 and 0057. The model
ships as one release.

## Considered options

- **Keep config grants automatic, deny from the CLI.** Rejected: a cloned or
  pulled project would still grant itself authority before anyone reads the
  config.
- **Deno's `-P` alone.** Rejected as the only mechanism: it applies whatever
  the file says at that moment, so a pull that widens permissions goes
  unnoticed. It remains as the explicit per-invocation form.
- **Config declares, only the CLI grants.** Rejected: CLI grants apply to every
  file in a run, so per-folder needs (the test suite's FFI and fetch folders)
  cannot be expressed.
- **A trust store keyed on content hash alone.** Rejected: a repository could
  copy a popular project's permission block and inherit its trust. Entries are
  keyed on path plus hash.
- **Named permission sets.** Rejected: per-folder configs already scope
  permissions, and named sets add a second axis to hash and trust.
- **`--allow-all` / `-A`.** Rejected: a sandbox-first runtime should not make
  "everything" a short flag; grants stay specific.
- **Interactive prompts.** Rejected: they make runs non-deterministic, hurt
  embedding and CI, and would pause the engine mid-execution.
- **A separate install step for provider packages.** Rejected once config can
  no longer grant `import` implicitly. Resolve-time materialization under an
  explicit grant keeps one command and verified offline reuse.
- **Keeping a separate sandbox binary.** Rejected: once the default runner has
  no ambient authority, the sandbox is a filesystem mode, not a different
  program.
- **Deprecation aliases for old flags.** Rejected: aliases would keep the
  ambiguous value-ignoring flags alive, and the project is pre-1.0.

## Consequences

- VISION's "no ambient file system" becomes literally true. Projects that read
  files outside their directory, or through dynamic imports, need `--allow-read`.
- CI adds `-P` to the test-runner commands. Contributors run
  `GocciaTestRunner --trust tests/` once per change to a permission block.
  Tests that fetch from `127.0.0.1` declare `private`.
- A capability can be traced to a flag, a trusted hash, `-P`, or an embedding
  API call, and the audit log records which.
- Trust covers what code may do, not what the code is. New code under an
  already-trusted block runs with those permissions.
- A single `goccia` binary with subcommands, which would give `--trust` a
  natural home as `goccia trust`, is a separate follow-up
  ([#1254](https://github.com/frostney/GocciaScript/issues/1254)).
- ADR 0107's out-of-process `capabilities` section should adopt these names.
- `docs/permissions.md` becomes the single reference; `docs/build-system.md`,
  `docs/capability-audit.md`, `docs/embedding.md`, and the website link to it.
