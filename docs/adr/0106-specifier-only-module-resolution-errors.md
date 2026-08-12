# Specifier-only module resolution errors

**Date:** 2026-08-12
**Area:** `modules`, `errors`, `security`
**Related:** [ADR 0092](0092-sandbox-filesystem-error-contract.md), [ADR 0103](0103-layered-untrusted-execution-boundaries.md)

## Context

`TModuleResolver.Resolve` built its failure messages out of the expanded host
filesystem candidate: `Module not found: "./x.js" (resolved to "/abs/dir/x.js")`
for relative specifiers, and `(alias resolved to "...")` for aliased ones. Those
messages did not stay on the host. The module loader rewraps a resolution
failure into a runtime error, the VM turns that into a JavaScript `Error`
object, and dynamic `import()` rejects its promise with the same text. Script
code could therefore read absolute host directory names out of `error.message`
by importing probe specifiers in a loop — an information disclosure that
survives every boundary ADR 0103 puts around untrusted execution, because the
leak originates inside the message the boundary is faithfully forwarding.

Sanitizing at the guest boundary would mean rewriting messages at each of the
several sites that convert a Pascal exception into a JavaScript value, and
every future conversion site would have to remember to do the same.

The same leak shape appeared twice more on the load path once resolution
succeeded: `import.source` of a resolvable non-script module and a JSON module
that fails to parse both formatted the expanded `ResolvedPath` into messages
that reach the identical guest catch-alls. The `import.source` one is reachable
in a single ungated import, because its raise precedes the
`--experimental-js-module-source` gate.

## Decision

Sanitize at the source. No module-loading failure message that can reach script
contains an expanded host path; the resolver carries its path in structured form
instead.

- Every `EModuleNotFound` that `TModuleResolver.Resolve` raises uses the message
  `Module not found: "<specifier>"`, with the specifier exactly as the import
  statement wrote it. The `(resolved to ...)` and `(alias resolved to ...)`
  suffixes are gone.
- The expanded candidate moves to a read-only `ResolvedCandidatePath` property
  on `EModuleNotFound`, set through the dedicated `CreateNotFound` constructor.
  Trusted hosts that catch the typed exception lose no diagnostic detail.
- The module loader forwards that field when it rewraps the failure, raising
  `TGocciaModuleResolutionError` (a `TGocciaRuntimeError` subclass) so the path
  survives to the CLI reporters without ever entering `Message`. Every host
  reporter renders engine errors through `FormatHostErrorDiagnostic`, which is a
  drop-in for `GetDetailedMessage` that appends a trailing
  `  Resolved to: <path>` line when the error carries one.
- The two non-resolution load failures — `import.source` of a non-script module
  and a JSON module parse error — name the specifier instead of the resolved
  path. They get no structured field: their expanded path already survives in
  the host-only `FileName` field, and a second carrier would not earn its
  weight.
- `TGocciaSandboxModuleResolver` is unchanged. It reports virtual filesystem
  paths, which are the guest's own namespace and already enumerable from guest
  code, so hiding them would cost diagnostics and buy nothing.
- The bare-specifier message and the no-resolver refusal already named no host
  path and keep their text.

## Consequences

Script-visible load failures are strictly less informative: a developer
debugging a broken import from inside JavaScript sees the specifier only, and
must look at the CLI output — or catch `EModuleNotFound` from an embedding
host — to learn which absolute path was probed. That asymmetry is the point.

The recovery is not uniform across output modes. Human-readable CLI output keeps
the candidate path; `--output=json` and `--output=compact-json` do not, because
their envelope is a documented contract of `type`, `message`, `line`, `column`,
and `fileName`, and extending it was judged a separate decision. Tooling that
consumes CLI JSON therefore loses the candidate path outright and should catch
the typed exception instead.

Anything asserting on the old message text has to change, including host tests
and tooling that scraped the parenthesized path out of `error.message`. The
structured field is the supported replacement.

This narrows one disclosure channel; it is not a general claim that engine error
messages are free of host detail. Other messages that embed host paths remain to
be audited on their own terms.
