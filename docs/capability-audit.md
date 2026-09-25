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
  "kind": "fetch.host",
  "decision": "deny",
  "subject": "blocked.example",
  "reason": "host is not in the allowed hosts list",
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

The current event kinds are:

| Kind | Meaning |
|------|---------|
| `fetch.host` | Fetch host allowlist decision |
| `fetch.dispatch` | An allowed fetch request reached the dispatch boundary |
| `ffi.open` | An installed FFI runtime accepted a library-open attempt |
| `function.constructor` | Dynamic Function construction was allowed or denied |
| `shadow-realm.construct` | An installed ShadowRealm constructor was invoked |
| `sandbox.fs.path` | A sandbox path attempted to cross above the virtual root |
| `modules.node-modules` | A host granted bare-specifier resolution against `node_modules` |

`fetch.host` subjects contain only the checked host. URL user information,
paths, and query parameters are not included in host-authorization events.

Disabled `FFI` and `ShadowRealm` remain absent globals. Auditing does not
install throwing stubs or alter feature detection. Their use events therefore
exist only when the host has installed those capabilities.

Sandbox root escapes retain their existing behavior: the escape portion is
denied and reported once for the path normalization request, while the
resulting in-jail path remains clamped and the filesystem operation continues.
Ordinary sandbox reads and writes do not generate events.

`modules.node-modules` is a configuration-time grant rather than a script
action: it is emitted once per engine when `--allow-node-modules` (or the
matching config key) is applied, with the effective ceiling directory as the
subject and an empty subject when the ancestor walk is unbounded. Individual
resolutions afterwards produce no events. See
[Module Resolution](module-resolution.md) for what the grant permits.

Fetch authorization and dispatch events are emitted synchronously on the
runtime thread. The HTTP worker does not call the sink.

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
