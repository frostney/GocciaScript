# Permissions

*The capability model: what an engine may reach outside its process, how grants and denies combine, and how a denial surfaces.*

## Executive Summary

- **One immutable set per engine** — `TGocciaCapabilities` grants `read`, `net`, `ffi`, and `import`; the engine receives it at construction and never changes it
- **Deny by default, deny wins** — an engine created without a set gets `None`, and a deny always beats an allow regardless of the order they were added in
- **The module graph is exempt** — static imports with literal specifiers of files inside the project need no `read` grant; every other host read does
- **Nested contexts only narrow** — ShadowRealm children, sandbox `runScript` children, and test262 realms inherit their parent's set and can never reach more
- **Denials are catchable and auditable** — a denied operation throws `PermissionDenied` naming the capability and the requested scope, never a host path, and emits an audit event

The design is recorded in [ADR 0122](adr/0122-unified-capability-model.md).
This page describes the engine mechanism. The command-line grammar that fills
the set (`--allow-<cap>` / `--deny-<cap>`) arrives in the next layer of that
work; until then the CLI builds the set from today's flags, as described in
[Command line](#command-line).

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
| `private` | lifts the private-range refusal; matches no destination by itself |

Host scopes are matched against the URL's host before any name lookup, so a
refused request never becomes an observable side effect. One trailing dot is
ignored on both sides, for names and IPv4 literals alike, so
`tracker.example.com.` is the host `tracker.example.com` and `127.0.0.1.` the
address `127.0.0.1`. An IP or CIDR scope matches only a URL that names an
address; host names are never resolved to match one. An IPv4-mapped IPv6
address (`::ffff:169.254.169.254`) is judged as the IPv4 address it names, and
an IP or CIDR scope also matches the IPv4 host a NAT64 (`64:ff9b::/96`) or 6to4
(`2002::/16`) address reaches: a deny on `169.254.169.254` covers
`64:ff9b::a9fe:a9fe` and `2002:a9fe:a9fe::1`.

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
`169.254.169.254` is refused unless `private` is allowed. Both checks run again
on every redirect hop.

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

Packages the import capability grants are part of the module graph (see
below): when a bare specifier resolves through a granted `node_modules` scope,
the package's canonical root — symlinks resolved, so a workspace or pnpm link
counts where it really lives — joins the graph. Provider packages will follow
the same rule.

## The module-graph exemption

Code needs to import its own files. Reads of the host filesystem made by a
**static import with a literal specifier** — including `json`, `text`, and
`bytes` imports, and `import()` whose specifier is a string literal — of a file
inside the **project** need no `read` grant. Neither does the file a literal
bare specifier resolved to through the `import` capability, nor a literal import
made by a file of that package that stays inside the package's canonical root.
A path that merely contains a `node_modules` segment is no grant: a relative or
absolute import of `./node_modules/x/file` is judged like any other path, by
where it canonically lives. The project is the directory of the
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
same file later is checked like any other guest read, cached or not.

For a relative or absolute specifier the resolver tries candidates in order —
the exact path, the extension variants, then `<path>/index.<ext>` — and each
candidate is judged, canonically, before the host is asked whether it exists. A
candidate the set refuses stops resolution with `PermissionDenied` there, so
whether a file the engine may not read exists can never decide between
`PermissionDenied`, "Module not found", or a later candidate loading. Deny
scopes that no candidate reaches do not matter: denying `lib.js.map` or
`lib-private` leaves `import "./lib"` alone, and a deny naming the directory
`lib` refuses `import "./lib"` because `lib` itself is the first candidate. The
path a request finally resolves to is judged again before any cache serves it.
`import.meta.resolve` runs the same judgment and, when a candidate is refused,
answers with the unprobed URL instead.

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
embedder — may name the canonical path and the option that would grant it.

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
| `.Grants`, `.Allows`, `.AllowsPath`, `.AllowsUnscoped`, `.AllowsNetHost`, `.AllowsNetAddress`, `.NodeModulesCeiling`, `.DeniesAll`, `.DeniesPath`, `.DeniesNodeModules`, `.AllowsProvider` | Queries |
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

The command-line flags change in the next layer of ADR 0122. Until then every
binary builds its set from today's flags with the precedence those flags
already have (command line, then per-file config, then root config), and
honors exactly the flags it honors today:

| Today's flag | Capability |
|---|---|
| host-filesystem module loading (the default) | `read` allowed everywhere |
| `--no-host-filesystem` | `read` denied outright |
| `--allowed-host=<host>` / `"allowed-hosts"` | `net` allowed for each value, plus `private`; any net scope is accepted (ports, `*.domain`, IPs, CIDR ranges) |
| `--fetch-deny-private-ranges` | `net` `private` denied |
| `--unsafe-ffi` | `ffi` allowed everywhere |
| `--allow-node-modules[=<dir>]` | `import` `node_modules[=<dir>]` |

The REPL and the benchmark runner ignore `--no-host-filesystem`, as before. The
sandbox runner loads no host files and grants only `net` and `ffi`. The
test262 runner, the bare loader, and the fuzz harness run core-language engines
with `None`.

## Related documents

- [ADR 0122](adr/0122-unified-capability-model.md) — the decision this page implements
- [Capability Audit Events](capability-audit.md) — the audit event contract
- [Module Resolution](module-resolution.md) — how specifiers become files
- [Fetch](built-ins-fetch.md) and [FFI](built-ins-ffi.md) — the capability-gated built-ins
- [Embedding](embedding.md) — the wider embedding API
