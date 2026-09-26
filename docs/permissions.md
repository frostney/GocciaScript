# Permissions

*The capability model: what an engine may reach outside its process, how grants and denies combine, and how a denial surfaces.*

## Executive Summary

- **One immutable set per engine** — `TGocciaCapabilities` grants `read`, `net`, `ffi`, and `import`; the engine receives it at construction and never changes it
- **Deny by default, deny wins** — an engine created without a set gets `None`, and a deny always beats an allow regardless of the order they were added in
- **The module graph is exempt** — static imports with literal specifiers of files inside the project need no `read` grant; every other host read does
- **Nested contexts only narrow** — ShadowRealm children, sandbox `runScript` children, and test262 realms inherit their parent's set and can never reach more
- **Denials are catchable and auditable** — a denied operation throws `PermissionDenied` naming the capability and the requested scope, never a host path, and emits an audit event
- **One command-line grammar** — `--allow-<cap>[=scope,...]` and `--deny-<cap>[=scope,...]`, a `permissions` block in config files, and `--max-*` limits with units

The design is recorded in [ADR 0122](adr/0122-unified-capability-model.md).
This page is the single reference for the engine mechanism, the
[command line](#command-line) and [config files](#config-files) that fill the
set, [limits](#limits-and-units), and the
[flags GocciaScript 0.14.0 removed](#removed-flags-and-keys).

## Capabilities

| Capability | Scopes | Covers |
|---|---|---|
| `read` | absolute paths, canonical and recursive | host file reads beyond the module-graph exemption |
| `net` | `host`, `host:port`, `*.domain`, IP, CIDR, `private` | `fetch`, checked on every redirect hop |
| `ffi` | absolute library paths | `FFI.open` and installing the `FFI` global |
| `import` | `node_modules`, `node_modules=<dir>`, provider hosts such as `github` | bare specifiers resolved against `node_modules`; provider imports are reserved |

An allow or deny with no scope covers every scope of that capability. Limits
such as the fetch response-body ceiling are settings, not capabilities: see
`FetchMaxResponseBytes` in [Embedding](#embedding).

## How a decision is made

A set is a stack of **layers**. The root layer is what the host granted; every
nested context appends a layer. A request is allowed only when:

1. no layer denies it — an unscoped deny, or a deny scope that covers the
   request; and
2. every layer allows it — an unscoped allow, or an allow scope that covers the
   request.

Because a deny is checked before any allow, it wins regardless of the order in
which the two were added, and a deny scope can carve an exception out of a
broader allow. Because every layer must allow, appending a layer can only
remove authority.

`TGocciaCapabilities.Grants(cap)` answers the coarser question "could any
request of this capability be allowed", which is how the runtime decides
whether to install the `FFI` global at all.

### `read` and `ffi` paths

A path scope must be absolute; a relative scope is rejected with
`EGocciaCapabilityScopeError`. Scopes and requests are canonicalized with
symbolic links resolved (`CanonicalHostPath`), so a scope spelled through a
link covers the directory it names, and a request cannot reach a denied
directory through a link. A path that does not exist yet is judged by its
deepest existing ancestor. Matching stops at a separator: `/a/b` covers
`/a/b/c.js` but not `/a/bc`.

`FFI.open("./lib.so")` is judged, and then loaded, at its canonical path, so
the file checked is the file opened. A bare library name such as `libc.so.6`
has no directory part and is searched for by the platform loader, which no
path scope can describe: it is allowed only when every layer allows `ffi`
unscoped and no layer has an `ffi` deny scope.

### `net` scopes and private ranges

| Scope | Matches |
|---|---|
| `api.example.com` | that host on any port |
| `api.example.com:8443` | that host on that port only |
| `*.example.com` | any subdomain of `example.com`, not `example.com` itself |
| `203.0.113.7`, `[2001:db8::1]:8080` | that address literal |
| `10.0.0.0/8`, `2001:db8::/32` | any address literal in the range |
| `private` | every private, loopback, and link-local destination |

Host scopes are matched against the URL's host before any name lookup, so a
refused request never becomes an observable side effect. One trailing dot is
ignored on both sides, so `tracker.example.com.` is the host
`tracker.example.com`. An IP or CIDR scope matches only a URL that names an
address; host names are never resolved to match one. An IPv4-mapped IPv6
address (`::ffff:169.254.169.254`) is judged as the IPv4 address it names.

Private, loopback, link-local, CGNAT, and similar ranges are denied unless they
are **named**: either the `private` scope is allowed, or the destination address
is covered by an explicit IP or CIDR allow. An unscoped allow does not name
them, but an explicit range does, however broad: `0.0.0.0/0` covers loopback,
RFC 1918, and the `169.254.169.254` metadata address too. A `private` **deny**
wins over every allow, explicit addresses and ranges included. IPv6 forms that
embed an IPv4 host — IPv4-compatible `::a.b.c.d`, NAT64 `64:ff9b::/96`, and
6to4 `2002::/16` — are private when the host they embed is.

This is checked twice: against an address literal in the URL, and against the
address a host name resolved to — so `api.example.com` whose DNS answers with
`169.254.169.254` is refused unless `private` (or `169.254.169.254`) is
allowed. Both checks run again on every redirect hop.

`private` is a grant of its own: `--allow-net=private` reaches
`http://127.0.0.1:8080` and `http://localhost:8080`, but no public address. A
host name that only `private` allows passes the check on its name and is
judged on the address it resolves to. A deny of `private`, or of an IP or CIDR
covering the address, refuses the destination whatever allows it.

### `import` scopes

`node_modules` allows bare specifiers to resolve by walking up from the
importing file's directory, as Node does; `node_modules=<dir>` bounds that walk
at `<dir>`, and applies only to importers inside it. The ceiling is compared
against expanded paths, as described in
[Module Resolution](module-resolution.md#enabling-node_modules). A bare
specifier the set does not grant keeps the sealed-by-default resolution
message; one it explicitly denies throws `PermissionDenied`.

Provider hosts (`github`) are modeled so a set can carry them, but provider
resolution is not implemented yet.

Files the import capability grants are part of the module graph (see below):
once a package is reached through a granted `node_modules` scope, its files
need no `read` grant, wherever that `node_modules` directory is. Provider
packages will follow the same rule.

## The module-graph exemption

Code needs to import its own files. Reads of the host filesystem made by a
**static import with a literal specifier** — including `json`, `text`, and
`bytes` imports, and `import()` whose specifier is a string literal — of a file
inside the **project**, or inside a `node_modules` directory the `import`
capability grants, need no `read` grant. The project is the directory of the
nearest `goccia.json`, `goccia.json5`, or `goccia.toml` above the entry file,
or the entry file's own directory when there is none (the `ProjectRoot` property of `TGocciaEngine`).

Everything else needs a `read` grant covering the canonical path:

- a static import of a file outside the project;
- a dynamic `import()` whose specifier is computed at run time, even of a file
  inside the project (`ShadowRealm.prototype.importValue` counts as computed);
- a `bytes` or data import outside the project.

A computed specifier is detected at compile time: the interpreter checks the
`import()` argument, and the bytecode compiler emits
`OP_COMPUTED_IMPORT_SPECIFIER` before the import opcode.

A **deny** removes the exemption too. A deny scope covering a project file
refuses even its static imports, and an unscoped `read` deny refuses every host
read. Either way the refusal is an audited `PermissionDenied`, like any other
denial.

Only reads through a content provider that reports `ReadsHostFileSystem` are
checked, so in-memory, archive, and sandbox-filesystem providers are unaffected.
Modules a host loads itself (`--globals`, `--modules`, `InjectModulesFromModule`
and their imports) are host requests and never checked; a guest importing the
same file later is checked like any other guest read, cached or not. Before the
resolver probes the host for a relative or absolute specifier the request is
checked against its lexical candidate — and refused when a deny scope names a
file the extension or index probe could reach — so a request the set refuses
cannot learn whether the file exists. For the same reason
`import.meta.resolve` answers with the unprobed URL for a path the engine may
not read.

## PermissionDenied

A denial throws a `PermissionDenied`, an `Error` subclass registered with the
core error constructors:

```javascript
try {
  await import(computedPath);
} catch (error) {
  error instanceof PermissionDenied; // true
  error.message;                     // "read: ./data/report.json"
  error.capability;                  // "read"
  error.scope;                       // "./data/report.json"
}
```

The message is `<capability>: <scope>`, where the scope is what the guest asked
for: the specifier as written, the host (plus a non-default port), or the
library path as passed to `FFI.open`. It never contains an expanded host path
([ADR 0108](adr/0108-specifier-only-module-resolution-errors.md)). The host-side
report — the CLI's `Suggestion:` line, or `TGocciaThrowValue.Suggestion` for an
embedder — names the canonical path and how to grant it: `--allow-read=<dir>`
or `"allow-read"` in a `permissions` block for a read the capability does not
cover (with a note that a computed `import()` is outside the module graph),
`--allow-ffi[=<dir>]` for a library, `--allow-net=<host>` or `private` for a
host, and the `--deny-*` or `deny-*` entry that refused a denied one.

## Audit events

Every decision that consults a capability emits a
[capability audit event](capability-audit.md), allow and deny alike:
`read.file`, `net.fetch`, `net.dispatch`, `ffi.open`, and `import.node-modules`
(`import.provider` is reserved). Exempt module-graph loads emit nothing. Each
root engine also emits one `capabilities.effective` event carrying
`TGocciaCapabilities.ToJSON`; nested contexts that inherit their parent's set —
ShadowRealm children and sandbox `runScript` children — report through the
same sink without repeating it. A fetch's address and redirect decisions are
attributed to the `fetch()` call that started it, even when they are delivered
later or after the request was aborted.

## Nested contexts

A nested context can only narrow its parent's set:

- ShadowRealm child realms are created with their creator's set and project.
- Sandbox `runScript` children inherit the running engine's set.
- test262 `$262.createRealm()` realms and agents inherit the case engine's set.

Worker threads and per-file engines (test runner, `--jobs`) are independent
roots, each built from the command line and its configuration.

## Embedding

```pascal
uses
  Goccia.Capabilities,
  Goccia.Engine,
  Goccia.Runtime,
  Goccia.RuntimeExtensions.FFI;

Capabilities := TGocciaCapabilities.None
  .Allow(gcRead, '/srv/app/data')
  .Deny(gcRead, '/srv/app/data/secrets')
  .Allow(gcNet, 'api.example.com')
  .Allow(gcImport, 'node_modules=/srv/app');

Engine := TGocciaEngine.Create('/srv/app/main.js', Source, Executor,
  Capabilities);
Runtime := AttachRuntime(Engine);   // filesystem provider, reads checked
InstallFFIIfGranted(Runtime);       // installs FFI only when ffi is granted
Engine.FetchMaxResponseBytes := 1024 * 1024;
```

| API | Purpose |
|---|---|
| `TGocciaCapabilities.None` / `.Unrestricted` | Grants nothing / everything including `private` (tests, fully trusted hosts) |
| `.Allow(cap, scope)` / `.Deny(cap, scope)` | Return a copy with the scope added to the innermost layer |
| `.Narrow(child)` | Return a copy with the child's layers appended |
| `.Grants`, `.Allows`, `.AllowsPath`, `.AllowsUnscoped`, `.AllowsNetHost`, `.AllowsNetAddress`, `.NodeModulesCeiling`, `.DeniesAll`, `.DeniesPath`, `.DeniesPathsStartingWith`, `.DeniesNodeModules`, `.AllowsProvider` | Queries |
| `.ExplainNetHostDenial` | The host-side reason a net host is refused, for audit |
| `.ToJSON` | The layers, as `capabilities.effective` reports them |
| `TGocciaEngine.Create(..., ACapabilities)` | Fixes the set; the overloads without one use `None` |
| `Engine.ProjectRoot` | The exemption's project directory; override before executing |
| `Engine.FetchMaxResponseBytes` | Response-body ceiling for `fetch` (0 = default) |
| `AttachRuntime(Engine)` | Installs the filesystem provider; the loader checks every read through it |
| `InstallFFIIfGranted(Runtime)` | Installs the FFI extension only when `ffi` is granted; installing it directly without the grant raises `EGocciaFFINotGranted` |

Every builder deep-copies the rules, so a value handed to an engine is never
affected by later builder calls on the original.

## Command line

Every binary fills the set from the same grammar:

```text
--allow-<cap>[=<scope>,<scope>...]
--deny-<cap>[=<scope>,<scope>...]
```

`<cap>` is `read`, `net`, `ffi`, or `import`. The flags repeat and
accumulate, and a deny wins regardless of order. A scope attaches only with
`=`: `--allow-read foo.js` is an unscoped grant followed by an input file.
Commas separate scopes, so a path containing a comma can only be named in a
config file. There is no `--allow-all` and no environment-variable form.

| Flag | Without a scope | With scopes |
|---|---|---|
| `--allow-read` | every path | paths, relative to the working directory |
| `--allow-net` | every public host | `host`, `host:port`, `*.domain`, IP, CIDR, `private` |
| `--allow-ffi` | every library (installs the `FFI` global) | library paths, relative to the working directory |
| `--allow-import` | not accepted: a scope is required | `node_modules`, `node_modules=<dir>`, a provider such as `github` |
| `--deny-read` | every read, including the project's own imports | paths |
| `--deny-net` | every host | as for `--allow-net` |
| `--deny-ffi` | every library | library paths |
| `--deny-import` | not accepted: a scope is required | as for `--allow-import` |

`--allow-net=` (an empty list) and `--allow-read=a,,b` (an empty item) are
invalid values (exit 1), as is a scope the capability does not accept. Every
binary parses these flags the same way, including `GocciaScriptLoaderBare`
and `GocciaTest262Runner`, which have their own argument parsers:

```text
Error: Invalid scope for --allow-net: "http://x" (use host, host:port, *.domain, an IP, a CIDR range, or private)
```

### Defaults

With no flags an engine reaches nothing beyond the
[module-graph exemption](#the-module-graph-exemption): static imports of files
inside the project work, and everything else is refused with
`PermissionDenied`:

```sh
GocciaScriptLoader app.js                        # imports inside the project only
GocciaScriptLoader app.js --allow-read=../shared # plus reads under ../shared
GocciaScriptLoader app.js --allow-net=api.example.com --allow-net=127.0.0.1
GocciaScriptLoader app.js --allow-import=node_modules
GocciaScriptLoader app.js --deny-read            # not even the project's imports
```

### Config files

A config file declares the permissions its files need in a `permissions`
object. Its keys are the eight flag names; each value is `true` (no scope),
`false` (absent, so a config can cancel what its `extends` base declared), or
an array of scopes:

```json
{
  "extends": "../goccia.json",
  "permissions": {
    "allow-read": ["../../fixtures/modules"],
    "allow-net": ["127.0.0.1", "example.com"],
    "allow-ffi": true,
    "allow-import": ["node_modules=."],
    "deny-net": ["10.0.0.0/8"]
  }
}
```

```toml
[permissions]
allow-ffi = ["../fixtures/ffi"]
```

- Relative path scopes, and the directory of `node_modules=<dir>`, resolve
  against the directory of the file that declares them.
- With `extends`, a child's key replaces its base's key; keys the child does
  not name are inherited.
- One config governs each file: its nearest `goccia.*`, or the root config
  when it has none. The root config is `--config`, or the one discovered by
  walking up from the first input's directory (the working directory for
  stdin and the REPL). Configs compose only through `extends`.
- An unknown key (`deny-nett`), `"allow-import": true`, or a value that is not
  `true`, `false`, or an array of strings (including `null`, an object, or a
  nested array) is a malformed block and fails the run with status 2. A
  scope the capability does not accept, or an empty scope, is an invalid
  value and fails with status 1, as on the command line.
- `allow-*` and `deny-*` at the top level of a config are errors: they belong
  in `permissions`.
- Every config governing a run's inputs is loaded and checked before any file
  runs, so a config error never leaves some files run and others not.

A command-line allow adds to a config's grants, and every deny, from either
source, subtracts. In this release a config's `permissions` block applies
without any further step; [ADR 0122](adr/0122-unified-capability-model.md)
adds a trust step for it.

### What each binary honors

A binary rejects an `--allow-*` flag for a capability it cannot grant (exit
2) and warns once when a config requests one:

```text
Error: GocciaSandboxRunner cannot grant read; it supports net. Remove --allow-read.
Warning: /repo/goccia.json requests allow-read, which GocciaBundler cannot grant; ignoring it
```

A `--deny-*` flag is always accepted. Limits follow the same rule on the
command line; a limit a binary does not apply is ignored in config, without
being validated.

| Binary | Capabilities | Limits | Config |
|---|---|---|---|
| `GocciaScriptLoader`, `GocciaTestRunner`, `GocciaBenchmarkRunner` | read, net, ffi, import | all | root and per-file |
| `GocciaREPL` | read, net, ffi, import | all, per evaluated input | discovered from the working directory |
| `GocciaBundler` | none | none | per-file, for compatibility flags |
| `GocciaSandboxRunner` | net | all, plus `--max-fs-bytes` and `--max-fs-nodes` | `--config` only |
| `GocciaScriptLoaderBare` | none | `--timeout`, `--max-memory`, `--max-instructions`, `--max-stack` | none |
| `GocciaTest262Runner` | none | `--timeout`, `--max-memory` | none |
| `GocciaWasmTestRunner` | read, net, ffi (not on LAKON) | none | per-file |

## Limits and units

Limits are settings, not capabilities. Each takes a unit:

| Option | Value | `0` means |
|---|---|---|
| `--timeout` | a duration: `500ms`, `5s`, `2m`, or plain milliseconds | no timeout |
| `--max-memory` | a size: `64MiB`, `1GiB`, or plain bytes | no ceiling |
| `--max-instructions` | a count | no limit |
| `--max-stack` | a count | no limit |
| `--max-fetch-bytes` | a size (default `8MiB`) | the default |
| `--max-fs-bytes`, `--max-fs-nodes` (sandbox) | a size (default `16MiB`) / a count (default 4096) | rejected |

Limits and flags in config files must have the right shape: a boolean flag
such as `"compat-var"` takes exactly `true` or `false` (not `"true"` or
`null`), and a limit takes a single value, not an array. Errors use the
config spelling, such as `Invalid value for "max-memory" in
/repo/goccia.json: 64MB ("MB" is ambiguous; ...)`, and exit 1.

Sizes accept `KiB`, `MiB`, and `GiB` (binary, case-insensitive, with or
without the `B`). `K`, `KB`, `M`, `MB`, `G`, and `GB` are rejected as
ambiguous, and so are fractions and signs. Duration units (`ms`, `s`, `m`)
are case-insensitive too. Config files take the same
spellings: `"timeout": "5s"`, `"max-memory": "64MiB"`, `"max-stack": 5000`. A
plain number keeps meaning milliseconds or bytes, so existing values still
work.

## Removed flags and keys

GocciaScript 0.14.0 removed the earlier capability flags without aliases.
Each now fails with status 2 and names its replacement:

| Removed | Replacement |
|---|---|
| `--allowed-host`, `"allowed-hosts"` | `--allow-net=<host>[,<host>...]`, `"permissions": { "allow-net": [...] }` |
| `--fetch-deny-private-ranges` | private ranges are denied by default; `--allow-net=private` allows them and `--deny-net=private` refuses them outright |
| `--fetch-max-response-bytes` | `--max-fetch-bytes` (units: `1MiB`) |
| `--unsafe-ffi` | `--allow-ffi[=<library>,...]`, `"permissions": { "allow-ffi": true }` |
| `--allow-node-modules[=<dir>]` | `--allow-import=node_modules[=<dir>]` |
| `--no-host-filesystem` | host reads are denied by default; `--deny-read` also refuses the project's imports |
| `--stack-size` | `--max-stack` |
| `--fs-quota-bytes`, `--fs-node-limit` (sandbox) | `--max-fs-bytes`, `--max-fs-nodes` |
| `--timeout-ms` (test262) | `--timeout` (units: `20s`) |

The config keys of the same names (`"unsafe-ffi"`, `"allow-node-modules"`,
`"no-host-filesystem"`, `"fetch-deny-private-ranges"`,
`"fetch-max-response-bytes"`, `"stack-size"`, and the sandbox runner's
`"fs-quota-bytes"` and `"fs-node-limit"`) fail the same way, naming the config
spelling of the replacement:

```text
Error: --unsafe-ffi was removed in GocciaScript 0.14.0; use --allow-ffi[=<library>,...] instead
Error: /repo/goccia.json: "allowed-hosts" was removed in GocciaScript 0.14.0; use "permissions": { "allow-net": [...] } instead
```

Boolean flags no longer take a value: `--compat-asi=false` is an error (exit
2) rather than a silent enable. In a config file a flag must be exactly `true`
or `false`; any other value is an invalid value (exit 1).

## Related documents

- [ADR 0122](adr/0122-unified-capability-model.md) — the decision this page implements
- [Capability Audit Events](capability-audit.md) — the audit event contract
- [Module Resolution](module-resolution.md) — how specifiers become files
- [Fetch](built-ins-fetch.md) and [FFI](built-ins-ffi.md) — the capability-gated built-ins
- [Embedding](embedding.md) — the wider embedding API
