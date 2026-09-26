# Permissions

<!-- doc-length-limit: 1000 -->
<!-- The single reference for grants, config trust, and sandbox mode (ADR 0122). -->

*The capability model: what an engine may reach outside its process, how grants and denies combine, and how a denial surfaces.*

## Executive Summary

- **One immutable set per engine** — `TGocciaCapabilities` grants `read`, `net`, `ffi`, and `import`; the engine receives it at construction and never changes it
- **Deny by default, deny wins** — an engine created without a set gets `None`, and a deny always beats an allow regardless of the order they were added in
- **The module graph is exempt** — static imports with literal specifiers of files inside the project need no `read` grant; every other host read does
- **Nested contexts only narrow** — ShadowRealm children, sandbox `runScript` children, and test262 realms inherit their parent's set and can never reach more
- **Denials are catchable and auditable** — a denied operation throws `PermissionDenied` naming the capability and the requested scope, never a host path, and emits an audit event
- **One command-line grammar** — `--allow-<cap>[=scope,...]` and `--deny-<cap>[=scope,...]`, a `permissions` block in config files, and `--max-*` limits with units
- **Config grants need trust** — a config's `allow-*` and `unsafe-*` requests apply only once the user trusts them (`--trust`) or accepts them for one run (`-P`); its denies always apply
- **Sandbox mode grants only `net`** — `GocciaRunner`'s sandbox mode sees only a virtual filesystem of copied inputs, and writes back to the host only inputs copied with `--copy-rw`, after a successful run

The design is recorded in [ADR 0122](adr/0122-unified-capability-model.md).
This page is the single reference for the engine mechanism, the
[command line](#command-line) and [config files](#config-files) that fill the
set, the [trust](#config-trust) config grants need,
[sandbox mode](#sandbox-mode), [limits](#limits-and-units), and the
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

`FFI.open("./lib.so")` is judged, and then loaded, at its canonical path. A
directory on that path could be swapped between the check and the load, so the
load is pinned to what was judged as far as the platform allows. On Linux the
file is opened first, the kernel's path for that descriptor is judged, and the
loader maps the descriptor itself, so the file checked is the file loaded.
On Windows the path the loader reports for the loaded module is judged again,
and a library outside the grant is unloaded and refused. On macOS and other
Unix systems the path is canonicalized again after the load and must still be
the judged one; a swap that is undone again within the load window cannot be
detected there. A bare library name such as `libc.so.6`
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
ignored on both sides, for names and IPv4 literals alike, so
`tracker.example.com.` is the host `tracker.example.com` and `127.0.0.1.` the
address `127.0.0.1`. An IP or CIDR scope matches only a URL that names an
address; host names are never resolved to match one. An IPv4-mapped IPv6
address (`::ffff:169.254.169.254`) is judged as the IPv4 address it names. An IP
or CIDR **deny** also matches the IPv4 host a NAT64 (`64:ff9b::/96`) or 6to4
(`2002::/16`) address reaches: a deny on `169.254.169.254` covers
`64:ff9b::a9fe:a9fe` and `2002:a9fe:a9fe::1`. An **allow** does not extend that
way — a 6to4 prefix names a relay site, not the IPv4 host, and NAT64 follows
the same rule — so allowing `10.0.0.5` does not allow `2002:a00:5::1` or
`64:ff9b::a00:5`; name those spellings explicitly to reach them.

Private, loopback, link-local, CGNAT, and similar ranges are denied unless they
are **named**: either the `private` scope is allowed, or the destination address
is covered by an explicit IP or CIDR allow. An unscoped allow does not name
them, but an explicit range does, however broad: `0.0.0.0/0` covers loopback,
RFC 1918, and the `169.254.169.254` metadata address too. An address scope with
a port names the address for that port only: `127.0.0.1:18765` lifts the
private refusal for port 18765, not for port 22, whether the URL names the
address or a host name that resolves to it. A `private` **deny**
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
and the imports they make while the host loads them) are host requests and never
checked. That exemption ends when the host's load returns: a function such a
module exports runs later as guest code, so an `import()` it makes then is a
guest read and is checked, and a guest importing the same file later is checked
like any other guest read, cached or not.

For a relative or absolute specifier — and for the path an alias or import map
rewrites a specifier to — the resolver tries candidates in order: the exact
path, the extension variants, then `<path>/index.<ext>`. Each candidate is
judged, canonically, before the host is asked whether it exists; the same
holds for the files a bare specifier's package target is probed as, where a
literal import's candidates inside that package are part of the graph. A
candidate the set refuses stops resolution with `PermissionDenied` there, so
whether a file the engine may not read exists can never decide between
`PermissionDenied`, "Module not found", or a later candidate loading, and a
`..` in an alias tail cannot probe past the alias target unjudged. Deny scopes
that no candidate reaches do not matter: denying `lib.js.map` or `lib-private`
leaves `import "./lib"` alone, and a deny naming the directory `lib` refuses
`import "./lib"` because `lib` itself is the first candidate.

Because the first candidate is the specifier's own path, a grant must cover it:
a file-level grant such as `read` on `/x/mod.js` admits `import("/x/mod.js")`
but refuses the extensionless computed `import("/x/mod")`, whose first candidate
`/x/mod` it does not cover. Grant the directory, or write the extension.

The path a request finally resolves to is judged again before any cache serves
it. `import.meta.resolve` runs the same judgment and, when a candidate is
refused, answers with the unprobed URL instead — for an aliased specifier, the
path the alias maps it to.

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
host. When a deny refused the request it names the deny instead: the read or
ffi deny covering the path, or the net deny scope that matched the host
(`refused by the net deny example.com`, `127.0.0.0/8`, or `private`), since
no allow can override it.
The suggestion also travels on the error object (never as a guest-visible
property), so a denial that surfaces through a rejected `import()` or
`fetch()` promise still reports it. Both executors locate a denial at the
guest request that caused it: the `import` or `export … from` declaration, the
`import()` expression, or the `fetch()`/`FFI.open()` call — each at the
position the parser records for that expression, which both executors report
identically.

## Audit events

Every decision that consults a capability emits a
[capability audit event](capability-audit.md), allow and deny alike:
`read.file`, `net.fetch`, `net.dispatch`, `ffi.open`, and `import.node-modules`
(`import.provider` is reserved). Exempt module-graph loads emit nothing. Each
root engine also emits one `capabilities.effective` event carrying
`TGocciaCapabilities.ToJSON`, whose reason is the set's provenance when the
host supplies one (`cli --allow-net=example.com; config /repo/goccia.json
trusted sha256:…`). The CLI emits one `config.permissions` event per config
that requests a grant, with the [trust](#config-trust) decision; nested contexts that inherit their parent's set —
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
| `Engine.CapabilityProvenance` | Where the host says the set came from; the reason of `capabilities.effective` |
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
binary parses these flags the same way and in the same order, including
`GocciaScriptLoaderBare` and `GocciaTest262Runner`, which have their own
argument parsers: a malformed flag is an invalid value (exit 1) even on a
binary that cannot grant the capability, and a well-formed `--allow-*` the
binary cannot grant is a usage error (exit 2):

```text
Error: Invalid scope for --allow-net: "http://x" (use host, host:port, *.domain, an IP, a CIDR range, or private)
```

### Defaults

With no flags an engine reaches nothing beyond the
[module-graph exemption](#the-module-graph-exemption): static imports of files
inside the project work, and everything else is refused with
`PermissionDenied`:

```sh
GocciaRunner app.js                        # imports inside the project only
GocciaRunner app.js --allow-read=../shared # plus reads under ../shared
GocciaRunner app.js --allow-net=api.example.com --allow-net=127.0.0.1
GocciaRunner app.js --allow-import=node_modules
GocciaRunner app.js --deny-read            # not even the project's imports
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
- A discovered config's `permissions` and `unsafe-*` keys govern only files
  inside its own directory tree. In `GocciaRunner a/x.js c/y.js`,
  `a/goccia.json` is the root config, but `c/y.js` (with no config of its
  own) gets no permissions or `unsafe-*` keys from it. Its other settings
  apply to every input as before. An explicit `--config` governs every
  input that has no config of its own. `GocciaTestRunner` given several
  inputs applies no root config at all, only each file's own.
- A file with its own config takes its `permissions` and `unsafe-*` keys from
  that config (and its `extends` chain) alone, even inside the root config's
  tree: a root key the file's config does not set is not inherited.
- An unknown key (`deny-nett`), `"allow-import": true`, or a value that is not
  `true`, `false`, or an array of strings (including `null`, an object, or a
  nested array) is a malformed block and fails the run with status 2. A
  scope the capability does not accept, or an empty scope, is an invalid
  value and fails with status 1, as on the command line.
- `allow-*` and `deny-*` at the top level of a config are errors: they belong
  in `permissions`.
- A file a config names as an input — a module manifest (`"modules"`), a
  globals file or module (`"globals"`), or a host-environment module
  (`"host-environment"`) — is read under the capability set of the script
  the config governs, with the config's own directory as the project: inside
  that directory it is covered as the module graph is, elsewhere it needs a
  read grant. A JavaScript or TypeScript manifest or globals module runs in an
  engine of its own and only data crosses back; a host-environment module is a
  guest module of the script's engine, never host-owned (see
  [Virtual Modules](virtual-modules.md)). The same options on the command line
  stay host requests.
- A config writes host files only inside its own directory: `log`,
  `audit-log`, `coverage-output`, `profile-output`, `source-map`, `output`,
  and the `sandbox` section's `copy-rw` inputs and `diff-file` resolve against
  the declaring file and fail with status 1 if they lead outside it, through a
  symbolic link or otherwise (see
  [Build System](build-system.md#configuration-file-gocciajson)). The check
  holds until the write: the file is opened from the config's directory, held
  to the identity it had when the config was read, one directory at a time
  without following links, so a directory swapped for a link while the script
  runs refuses the write instead of redirecting it (for the sandbox's inputs
  and diff file, see [Write-back](#write-back)).
- Every config governing a run's inputs is loaded and checked before any file
  runs, so a config error never leaves some files run and others not.

A command-line allow adds to a config's grants, and every deny, from either
source, subtracts. A config's `allow-*` keys, and its top-level
`unsafe-function-constructor` and `unsafe-shadowrealm` keys, take effect only
once the config is trusted; see [Config trust](#config-trust). Its `deny-*`
keys always apply.

### What each binary honors

A binary rejects an `--allow-*` flag for a capability it cannot grant (exit
2) and warns once when a config requests one:

```text
Error: GocciaBundler cannot grant read; it supports no capability flags. Remove --allow-read.
Warning: /repo/goccia.json requests allow-read, which GocciaBundler cannot grant; ignoring it
```

A `--deny-*` flag is always accepted. Limits follow the same rule on the
command line; a limit a binary does not apply is ignored in config, without
being validated.

| Binary | Capabilities | Limits | Config |
|---|---|---|---|
| `GocciaRunner` (host mode), `GocciaTestRunner`, `GocciaBenchmarkRunner` | read, net, ffi, import | all | root and per-file |
| `GocciaRunner` ([sandbox mode](#sandbox-mode)) | net | all, plus `--max-fs-bytes` and `--max-fs-nodes` | root, including its `sandbox` section |
| `GocciaREPL` | read, net, ffi, import | all, per evaluated input | discovered from the working directory |
| `GocciaBundler` | none | none | per-file, for compatibility flags |
| `GocciaScriptLoaderBare` | none | `--timeout`, `--max-memory`, `--max-instructions`, `--max-stack` | none |
| `GocciaTest262Runner` | none | `--timeout`, `--max-memory` | none |
| `GocciaWasmTestRunner` | read, net, ffi (not on LAKON) | none | per-file, accepted with `-P` |

## Config trust

A config file can sit in any repository, so what it asks for is a request,
not a grant. A config's **grants** — its `allow-*` permissions and its
`unsafe-function-constructor` and `unsafe-shadowrealm` keys — take effect only
once the user trusts them. Everything else applies automatically: `compat-*`,
`experimental-*`, limits, import maps, aliases, and every `deny-*` permission.
A block that only denies needs no trust.

```sh
GocciaTestRunner --trust tests/        # review and trust each config under tests/
GocciaTestRunner tests                 # runs with the trusted grants
GocciaTestRunner -P tests              # CI: accept every request for this run only
```

### When trust is checked

Before any file runs, the binary collects the config that governs each input
(its nearest `goccia.*`, else the root config) and checks each one that
requests a grant. Standard input is governed by the working directory's config,
the REPL checks the working directory's config before its prompt, and
`GocciaRunner` also checks a root config with a `sandbox` section. If any is
untrusted the run stops
with status 2 and nothing runs:

```text
Error: 2 config files request permissions that have not been trusted:

  tests/built-ins/fetch/goccia.json (never trusted)
    allow-net: 0.0.0.0, 127.0.0.1, example.com

  tests/built-ins/FFI/goccia.json (changed since trusted 2026-09-20T10:12:03Z)
    allow-ffi: /home/u/GocciaScript/fixtures/ffi
  + allow-read: /home/u/GocciaScript/fixtures/modules

Nothing was run. To trust these requests (stored in /home/u/.config/goccia/trust.json):
  GocciaTestRunner --trust tests/built-ins/fetch/goccia.json --trust tests/built-ins/FFI/goccia.json
To accept them for this run only:
  GocciaTestRunner -P tests --mode=bytecode
To run with command-line grants only:
  GocciaTestRunner --ignore-config-permissions tests --mode=bytecode
```

Paths under the working directory are shown relative to it, the suggested
commands repeat the real arguments, and more than three configs are trusted
through their nearest common directory. A changed config shows each line of
its block: unchanged lines indented, added lines with `+`, removed lines with
`-`.

Trust is only checked when the binary honors at least one requested grant. A
request it cannot honor is a warning instead (see
[What each binary honors](#what-each-binary-honors)): `GocciaBundler` runs no
code, so a config with grants only warns there.

### Trusting, listing, and removing

| Option | Effect |
|---|---|
| `--trust <path>` | Shows the requests of each config at or under `<path>` (a config file, or a directory scanned for the effective `goccia.toml`, `goccia.json5`, or `goccia.json` of each folder, skipping `node_modules` and `.git`) with what changed since it was trusted, asks for confirmation, and records them. Repeatable. |
| `--yes` | Confirms `--trust` without a prompt. Without a terminal, `--trust` needs it and otherwise fails with status 2, leaving the store unchanged. |
| `--untrust <path>` | Removes the entries for configs at or under `<path>`. Repeatable. |
| `--list-trusted` | Lists each entry with when it was trusted and its keys, marked `(changed)` when the config's block has changed or `(missing)` when the file is gone. |
| `-P`, `--accept-config-permissions` | Applies every config request for this run without trusting it, and never reads or writes the store. |
| `--ignore-config-permissions` | Applies no config grant for this run; the config's denies still apply. |
| `--trust-store=<path>` | Uses this store file instead of the per-user one. |

`--trust`, `--untrust`, and `--list-trusted` run on their own: combining two
of them, or one with input files, `-P`, or `--ignore-config-permissions`, is a
usage error, as is `-P` with `--ignore-config-permissions`. So is an empty
`--trust` or `--untrust` path (`--untrust=` would otherwise mean the working
directory), `-P` given a value (`-P=1`), and `--trust-store` without `=`: the
store path attaches only as `--trust-store=<path>`, so it never takes an
input file as its value. A `--trust` path that does not exist fails with
status 1. All of these
options are command-line-only; in a config file they fail with status 2. There
is no environment variable for the store: one set ambiently, by a repository's
tooling for example, could point at a store the repository pre-trusted.

With the command line, the precedence is:

- a command-line allow always applies;
- a config's grants apply when the config is trusted or `-P` is given;
- every deny subtracts, from the command line or from the config, trusted or
  not, and even under `--ignore-config-permissions`.

`--unsafe-function-constructor` and `--unsafe-shadowrealm` on the command line
need no trust; in a config they are requests like any allow.

### What a trust covers

Each entry records the config's canonical path and the SHA-256 of its
**normalized block**: the effective permission request after `extends`
resolution, as canonical JSON.

```json
{"permissions":{"allow-ffi":["/abs/fixtures/ffi"],"allow-net":["0.0.0.0","127.0.0.1","example.com"]},"unsafe":{"unsafe-function-constructor":true},"version":1}
```

- Keys are sorted in byte order; each capability's scopes are deduplicated and
  sorted, and a capability with an unscoped entry collapses to `true`. `false`
  and empty keys are omitted, and `unsafe` lists only keys set to `true`.
- Relative paths are made absolute against the file that declares them, with
  no trailing separator. This is lexical: symbolic links are not resolved, so a
  trust stays valid when a path it names (a built library, say) appears later.
- Net scopes are lowercased, and `node_modules` and provider names too.
- Denies are part of the block even though they need no trust, so a trusted
  block is exactly the one reviewed.

Because the key is the path as well as the hash, a block copied to another
path is not trusted, and because a base config is part of its child's block, a
change to a base reached through `extends` invalidates every child. Trust
covers what code may do, not what the code is: new code under a trusted block
runs with its permissions.

The block is lexical, so replacing a trusted path with a symbolic link would
keep its hash while pointing the grant somewhere else. Each entry therefore
also records, outside the hash, where every path scope (`read` and `ffi`
paths, allow and deny, and the directory of `node_modules=<dir>`) resolved
when it was trusted: the scope with links resolved, or, when it did not exist,
its deepest existing ancestor resolved plus the rest of the path. A scope that
now resolves
to a different place makes the config changed since trusted, and the report
and `--list-trusted` show it:

```text
  project/goccia.json (changed since trusted 2026-09-20T10:12:03Z)
    allow-read: /home/u/project/data
  ~ target of /home/u/project/data: /home/u/project/data -> /etc
```

So re-pointing any existing part of a scope's path is a change, even for a
scope that never existed: `allow-read: ./cfg/ssh` with no `cfg/` changes when
`cfg` appears as a link to `/etc`. The scope itself appearing in place, as a
build output does, resolves where it was recorded and is not a change, and
neither is a scope that disappears.

### The store

| Platform | Path |
|---|---|
| Linux and BSD | `$XDG_CONFIG_HOME/goccia/trust.json`, or `~/.config/goccia/trust.json` |
| macOS | `~/Library/Application Support/Goccia/trust.json` |
| Windows | `%APPDATA%\Goccia\trust.json` |
| LAKON | none; `GocciaWasmTestRunner` takes `-P` |

```json
{
  "version": 1,
  "trusted": {
    "/abs/tests/built-ins/fetch/goccia.json": {
      "sha256": "9f2c…",
      "block": {"permissions":{"allow-net":["0.0.0.0","127.0.0.1","example.com"]},"version":1},
      "targets": {},
      "trustedAt": "2026-09-25T10:12:03Z",
      "trustedBy": "GocciaTestRunner 0.14.0"
    }
  }
}
```

Keys are compared case-insensitively on macOS and Windows. The stored `block`
is only used to show what changed; grants always come from the current file,
and only when its hash matches. The directory is created private to the user
(`0700`), and the per-user default directory is made private again if it is
not; a `--trust-store` directory is left as it is. The file is written `0600`
from creation, before it replaces the store. A run reads the store once and
never locks it. A writer takes an exclusive operating-system lock on
`trust.json.lock` (`flock` on Linux and macOS, `LockFileEx` on Windows),
retrying for 2 seconds and then failing with an error that names the lock
file. It applies its changes to the store as it is on disk at that moment and
replaces the file in one rename, so concurrent readers and writers always see
a whole store and no writer's change is lost. The system releases the lock
when its writer exits, even by crashing, so a `trust.json.lock` left on disk
never blocks the next writer.

A missing store is empty. A store that is not JSON, that has the wrong shape
(anything but the schema above: a missing or non-integer `version`, a
`trusted` that is not an object, an entry missing a field or with a field of
the wrong type or an unknown key (`targets` is optional; its values are
strings), a `sha256` that is not 64 lower-case hex
digits), or that a newer GocciaScript wrote, is an error with status 1 for
`--trust`, `--untrust`, and `--list-trusted`, which never overwrite it; at run
time its configs are treated as untrusted and the report names the problem:

```text
Error: trust store /home/u/.config/goccia/trust.json is not valid JSON; fix or delete it
Error: trust store /home/u/.config/goccia/trust.json is not a valid trust store (no "trusted"); fix or delete it
Error: trust store /home/u/.config/goccia/trust.json was written by a newer GocciaScript (version 2); upgrade GocciaScript or remove the file
```

A store that cannot be located (`cannot locate the per-user trust store (HOME
is not set); pass --trust-store=<path>`), a held lock, or a failed write is
also an error with status 1.

### `GocciaWasmTestRunner`

The Wasm test runner has no store: it takes `GocciaWasmTestRunner [-P]
<manifest>`. Without `-P`, each file whose config requests a grant fails with
`FILEERROR <file> :: <config> requests permissions; pass -P to accept them
(GocciaWasmTestRunner has no trust store)`. Extra arguments after the
manifest are ignored with a warning, for the external harness.

## Sandbox mode

`GocciaRunner` has two modes. In **host mode**, the default, source runs
against the host filesystem under the set this page describes. In **sandbox
mode** the entry runs inside an isolated in-memory virtual filesystem, filled
only with what the host copies in, and can import `"fs"` and `"goccia"`
([Sandbox Modules](built-ins.md#sandbox-modules-gocciaruntimeextensionssandboxpas)).
`--sandbox`, any `--copy` or `--copy-rw` input, or a trusted
[`sandbox` config section](#the-sandbox-config-section) turns sandbox mode on.
The options and diff output are listed under
[Build System — GocciaRunner sandbox mode](build-system.md#gocciarunner-sandbox-mode).

### What sandbox mode honors

The guest reaches files only through the virtual filesystem, so `net` is the
one capability sandbox mode grants. `--allow-net` and `--deny-net` apply as in
host mode. `--allow-read`, `--allow-import`, and `--allow-ffi` are usage errors
(exit 2); `--deny-read`, `--deny-import`, and `--deny-ffi` are accepted and
change nothing. A config that requests `read`, `import`, or `ffi` gets a
warning instead:

```text
Error: --allow-read cannot be used in sandbox mode (enabled by --copy): the sandbox has no host filesystem; copy inputs with --copy
Warning: /repo/goccia.json requests allow-read, which GocciaRunner sandbox mode cannot grant; ignoring it
```

Nested `runScript` children inherit the running engine's set, as described in
[Nested contexts](#nested-contexts).

### Copy inputs

`--copy <host>[=<sandbox>]` copies a host file or directory into the sandbox
before the run, read-only. `--copy-rw` copies the same way and marks the input
for [write-back](#write-back). Both repeat. The copies are snapshots, not live
mounts: they form the baseline a diff is measured against, and the host path is
never reachable from the guest.

- A host path on the command line is relative to the working directory; one in
  the `sandbox` section is relative to the config file that declares it.
- The default target is `/<basename>`, for files and directories alike, so
  `--copy src` lands at `/src`. `dir=/x` copies the directory's contents into
  `/x`, and `dir=/` into the root.
- A target is an absolute sandbox path. An empty target (`x=`), a relative one
  (`x=rel`), or one that climbs above the root (`x=/../etc`) is a usage error
  (exit 2); so are two command-line inputs, or two config inputs, that land on
  the same path.
- A file whose target ends in `/`, or names a directory that already exists, is
  copied inside it.
- A host path with no basename (a filesystem root) is an error that asks for
  an explicit `=<sandbox>` target.
- Symbolic links are refused rather than followed, anywhere in a copied tree:
  `Copy path is a symlink (not supported): <path>` (exit 1, see
  [ADR 0071](adr/0071-reject-symlinks-in-sandbox-seed-imports.md)).

The positional entry file is copied read-only to `/<basename>` — unless it lies
inside a copied input, in which case it runs at that input's sandbox path and is
not copied again: `GocciaRunner src/main.js --copy src` runs `/src/main.js`. An
entry that would land on another input's path is an error that suggests
`--entry` or an explicit target. A positional entry that is a symbolic link is
refused like any other copy. `--entry=<sandbox-path>` runs a path that only
exists in the sandbox, and must be absolute; giving both a positional file and
`--entry` is a usage error.

Sandbox mode sees only the virtual filesystem. A root config's `modules`,
`module`, `globals`, `global`, and `host-environment`, which read host files,
are not applied: each gets one warning, such as
`Warning: <config> sets "modules", which GocciaRunner sandbox mode does not
apply; ignoring it`. The command line's `--module` and `--modules` still
apply: a module or manifest named there is the user's explicit choice, not the
repository's.

### Write-back

Nothing the guest writes reaches the host unless the host asked for it. After a
**successful** run, files changed under a `--copy-rw` input are written back to
the host paths they were copied from, including new files created inside a
`--copy-rw` directory. The rules of
[ADR 0119](adr/0119-host-applied-sandbox-write-back.md) hold:

- a failed run writes nothing;
- changes under a read-only `--copy` input, or outside every input, are
  reported as skipped;
- a deletion is never applied;
- a host target that is a symbolic link is skipped;
- each file is written to an exclusively created temporary beside it and then
  replaces it in one rename, so a failed write leaves the original intact.

Each input's directory is pinned: a command-line input when it is copied (its
canonical path and, on POSIX, its device and inode), and a config-named one
when the config is checked, as the config's directory plus the route from it.
Before anything is written, every read-write input must still be reachable as
pinned; one that was moved, or swapped for a symbolic link (an ancestor on a
config input's route included), means nothing is written. Each write then
walks down from the pinned directory without following a link (on POSIX
through directory descriptors opened with `O_NOFOLLOW`), creating missing
directories, so a link planted at any point after the check cannot carry a
write out. A config's `diff-file` is pinned the same way, from the config's
directory, and a link at its own name is refused. A `--diff-file` on the
command line is the user's own choice and is written as named — `/dev/null`, a
FIFO, or a link the user chose — replacing a regular file atomically. A write
that fails or is refused makes the run exit 1.

The report, one `write-back:` line per path, goes to standard error so the
guest's standard output stays clean.

### The `sandbox` config section

The root config can declare a sandbox run, so a repository can describe its
inputs once:

```json
{
  "sandbox": {
    "copy": ["src", "fixtures=/data"],
    "copy-rw": ["out"],
    "diff": "unified"
  },
  "max-fs-bytes": "32MiB"
}
```

```toml
[sandbox]
copy = ["src", "fixtures=/data"]
copy-rw = ["out"]
```

| Key | Value |
|---|---|
| `copy`, `copy-rw` | Arrays of `<host>[=<sandbox>]` strings, as on the command line, relative to the declaring config file; a single string is an error |
| `entry` | An absolute sandbox path, as `--entry`; a positional entry on the command line wins, with a note on stderr |
| `diff` | `true`, `"json"`, or `"unified"` |
| `diff-file` | A host path, as `--diff-file`, relative to the declaring config file |

- Any other key is an error that lists these; `files`, the old seed-config
  shape, is an error that names `copy` and `copy-rw`. Inline text and base64
  entries are gone.
- An empty section, `"sandbox": {}`, turns sandbox mode on with no inputs.
- Only the root config's section is read: `--config`, or the one discovered
  from the entry file (from the working directory when `--entry` names the
  entry). A section in any other config, the entry's own config under
  `--config=<other>` included, is ignored with the warning
  `declares a "sandbox" section, which GocciaRunner reads only from the root
  config; ignoring it`, and needs no trust.
- `max-fs-bytes` and `max-fs-nodes` are limits at the top level of the root
  config, not part of the section. Sandbox mode reads and validates them; host
  mode ignores them without validating them.

**Trust.** A config that can copy host files into a run and write results back
asks for authority, so its `sandbox` section is a request like an `allow-*`
key: `GocciaRunner` applies it only once the config is trusted (`--trust`) or
accepted for the run (`-P`), and stops with status 2 otherwise, as described in
[When trust is checked](#when-trust-is-checked). Binaries that do not run
sandbox mode warn that they ignore the section instead of asking for trust.

**Confinement.** The host paths a config can write — its `copy-rw` inputs and
its `diff-file` — must lie inside the declaring config's own directory tree,
judged canonically, so a symbolic link cannot lead out of it; otherwise the run
fails with status 1, naming the key and the config. The same values on the
command line may name any path.

**Combining with the command line.** Command-line `--copy` and `--copy-rw`
inputs add to the section's. When a command-line input has the same sandbox
target as a config input, the command-line one replaces it, so `--copy out`
over a config's `"copy-rw": ["out"]` is a read-only dry run. There is no flag
that turns a config's sandbox off; run with `--config=<other>` to leave it, or
with `--ignore-config-permissions`, which ignores the section along with the
config's other requests.

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
| `--seed <host>[=<sandbox>]` (sandbox) | `--copy <host>[=<sandbox>]`; the default sandbox path is now `/<basename>`, for directories too |
| `--seed-config` (sandbox) | the [`sandbox` section](#the-sandbox-config-section) of `goccia.json`, or `--copy`; inline text and base64 entries are gone |
| `--write-back` (sandbox) | copy the inputs that may be written with `--copy-rw` |
| `--diff-format=<format>` (sandbox) | `--diff=json\|unified` |
| `--diff-output=<path>` (sandbox) | `--diff-file=<path>` |
| `--diff-metadata` (sandbox) | none: JSON diffs always include timestamp metadata |
| `--timeout-ms` (test262) | `--timeout` (units: `20s`) |

The config keys of the same names (`"unsafe-ffi"`, `"allow-node-modules"`,
`"no-host-filesystem"`, `"fetch-deny-private-ranges"`,
`"fetch-max-response-bytes"`, `"stack-size"`, and the old sandbox runner's
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
