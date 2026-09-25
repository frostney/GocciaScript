# Capability-gated provider imports materialize verified local packages

**Date:** 2026-07-25
**Area:** `modules`, `host capabilities`
**Issue:** [#1053](https://github.com/frostney/GocciaScript/issues/1053)
**Related:** [ADR 0097](0097-engine-owned-capability-audit-seam.md)

GocciaScript keeps ordinary module loading local. A provider-qualified entry
in an import map is handled during resolver configuration: after an explicit
remote-import capability decision, a provider materializer reads a committed
lockfile, verifies or fetches the selected artifacts into a local cache, and
returns the cached package entry as an ordinary file path. The module loader
and module content provider remain unaware of remote transport.

The first provider is GitHub and accepts
`github:owner/repository@requested-ref`. A sibling `goccia.lock.json` maps that
identity to an exact Git commit, a package entry, and SHA-256-pinned artifacts.
Provider URLs are derived from those fields; import maps and lockfiles cannot
supply arbitrary URLs. Artifacts without a platform apply everywhere, while a
`<build-os>-<build-arch>` selector includes platform-native libraries in the
same verified package cache.

Authorization precedes cache inspection. Every remote entry is denied unless
the host grants `--remote-imports`, even if all required bytes are already
cached. The resolver emits `remote-import.resolve` for that decision.
Script-level `fetch` remains a separate GET/HEAD runtime surface with its own
host allowlist and audit events. FFI remains a separate capability when a
package later opens a cached native library.

Alternatives considered:

- **Load remote content lazily through the module content provider.** Rejected
  because it makes ordinary module evaluation own network and cache policy,
  and makes offline behavior depend on the loader path.
- **Accept raw HTTPS import-map addresses.** Rejected because arbitrary URLs
  do not carry provider identity, an immutable reference, or the complete
  artifact set needed for native packages.
- **Use Node/npm package resolution.** Rejected because Node host
  compatibility is outside the project vision and would add ambient package
  and filesystem conventions.
- **Authorize only cache misses.** Rejected because cached remote packages are
  still authority selected by a remote import-map entry; cache state must not
  bypass the host decision.
- **Require a separate install command before execution.** Rejected for the
  first vertical slice because import-map resolution can materialize the exact
  same locked bytes while retaining verified offline reuse. Lockfile updates
  remain an explicit maintenance action.

Consequences:

- Local import-map entries and module loading retain their existing offline
  path and behavior.
- A complete valid cache runs without network access but never without the
  remote-import capability.
- The runtime does not create or update lockfiles, resolve floating provider
  refs, run package scripts, or search Node/npm layouts.
- Provider expansion requires a new provider implementation that derives its
  own GET URLs from validated lock data; it does not widen the import-map
  address grammar to raw URLs.
- `GocciaSandboxRunner` keeps its isolated virtual filesystem and does not
  expose the host-backed remote package cache in this slice.
