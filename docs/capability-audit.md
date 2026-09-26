# Capability Audit Events

GocciaScript can report structured events when source reaches a
host-controlled capability boundary. Embedders install a sink on
the `CapabilityAuditSink` property of `TGocciaEngine`; CLI hosts use
`--audit-log=<path>` to write the same events as UTF-8 JSON Lines.

## Event Contract

Every event has schema version `1` and these fields:

```json
{
  "schemaVersion": 1,
  "kind": "net.fetch",
  "decision": "deny",
  "subject": "blocked.example",
  "reason": "the net capability does not allow port 443 of this host",
  "source": {
    "file": "app.js",
    "line": 12,
    "column": 5
  }
}
```

`decision` records the capability decision, not whether the requested
operation eventually succeeded. For example, `ffi.open` reports `allow` before
the dynamic library is loaded; a missing library may still fail afterward.
Line and column are `null` only when an event originates outside an active
source call site.

Kinds named after a [capability](permissions.md) report every decision that
consulted it, allow and deny alike:

| Kind | Meaning |
|------|---------|
| `capabilities.effective` | The engine's capability set, as JSON in `subject`; once per root engine |
| `read.file` | A host read that needed the `read` capability; `subject` is the canonical path |
| `net.fetch` | A `net` decision for a fetch destination: the host name, the address it resolved to, and every redirect hop |
| `net.dispatch` | An allowed fetch request reached the dispatch boundary |
| `ffi.open` | An `ffi` decision for a library-open attempt |
| `import.node-modules` | An `import` decision for a bare specifier resolved against `node_modules` |
| `import.provider` | Reserved for provider imports |

The remaining kinds report engine features that are not capabilities:

| Kind | Meaning |
|------|---------|
| `function.constructor` | Dynamic Function construction was allowed or denied |
| `shadow-realm.construct` | An installed ShadowRealm constructor was invoked |
| `sandbox.fs.path` | A sandbox path attempted to cross above the virtual root |
| `config.permissions` | A host's [trust](permissions.md#config-trust) decision on a config's permission requests; `subject` is the config path |

The CLI emits one `config.permissions` event per config that requests a grant,
before any file runs and before the engines' own events. `allow` reasons are
`trusted sha256:<hex>`, `accepted for this run (-P)`, and `not needed:
<Program> honors none of these requests`; `deny` reasons are `not trusted`,
`changed since trusted`, and `ignored (--ignore-config-permissions)`. A denied
config still contributes its `deny-*` permissions.

`net.fetch` subjects contain only the checked host. URL user information,
paths, and query parameters are not included in host-authorization events.
Static imports inside the project are exempt from `read`
([the module graph](permissions.md#the-module-graph-exemption)) and emit
nothing.

Disabled `FFI` and `ShadowRealm` remain absent globals. Auditing does not
install throwing stubs or alter feature detection. Their use events therefore
exist only when the host has installed those capabilities.

Sandbox root escapes retain their existing behavior: the escape portion is
denied and reported once for the path normalization request, while the
resulting in-jail path remains clamped and the filesystem operation continues.
Ordinary sandbox reads and writes do not generate events.

`capabilities.effective` is emitted when a host calls
`AuditEffectiveCapabilities` after installing the sink (the CLI does), or
otherwise just before the engine's first other event. Child contexts that
inherit the set — ShadowRealm realms and sandbox `runScript` children — do not
repeat it. Its `subject` is
`TGocciaCapabilities.ToJSON`: one object per layer, each with `allowAll`,
`allow`, `denyAll`, and `deny` for every capability. Its `reason` is the
engine's `CapabilityProvenance` when the host sets one; the CLI records the
command-line grants and the governing config's trust decision, such as
`cli --allow-read=data; config /repo/goccia.json trusted sha256:<hex>`, or
`defaults` when neither contributes.

`import.node-modules` subjects are the bare specifier; the reason carries the
ceiling the walk was bounded by. See [Module Resolution](module-resolution.md)
for what the grant permits.

Every event is delivered on the runtime thread. The decisions a fetch worker
makes for resolved addresses and redirect hops are recorded with the request
and delivered when its completion arrives — also after an abort, until the
engine discards its requests — attributed to the `fetch()` call that started
it. The HTTP worker never calls the sink.

## Embedding

The sink is a Pascal method callback:

```pascal
uses
  Goccia.CapabilityAudit,
  Goccia.Engine;

procedure TMyHost.HandleCapabilityAudit(
  const AEvent: TGocciaCapabilityAuditEvent);
begin
  WriteLn(AEvent.ToJSON);
end;

Engine.CapabilityAuditSink := HandleCapabilityAudit;
```

Install the sink before executing source or attaching runtime capabilities.
ShadowRealm child engines inherit the parent engine's sink.

Sink exceptions propagate as `EGocciaCapabilityAuditDeliveryError` and stop
execution. Script-level `try`/`catch` and promise rejection handlers cannot
intercept delivery failures. Hosts must treat delivery as part of the security
contract: serialize concurrent calls when engines run on multiple threads, and
do not swallow storage or transport failures.

## CLI

All `TGocciaCLIApplication`-based hosts accept:

```bash
./build/GocciaScriptLoader app.js --audit-log=capabilities.jsonl
./build/GocciaSandboxRunner /main.js \
  --seed-config=sandbox.json \
  --audit-log=capabilities.jsonl
```

The file is created before execution. Failure to open it or write an event is
fatal. Writes are serialized, so batch hosts may safely combine events from
`--jobs=N` workers into one JSONL stream. `--log` and `--audit-log` must name
different files; the CLI rejects equivalent expanded paths before opening
either output.
