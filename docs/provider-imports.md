# Provider Imports

*How an import-map entry naming a `github:` package becomes verified local files, what the `import` capability grants for it, and the rules a package's own files follow.*

## Executive Summary

- **Declared in the import map** — `"raylib": "github:<owner>/<repo>@<tag-or-commit>/<path>"` (an exact entry) or `"raylib/": "github:…@<ref>/<dir>/"` (a prefix entry) names a provider package; there is no separate package manifest
- **Pinned by `goccia.lock.json`** — the lockfile beside the import map pins each package to a commit and every file to a SHA-256; a run reads it and never writes it
- **Granted by `import` scopes** — `--allow-import=github`, `github:<owner>`, or `github:<owner>/<repo>`; a deny from any source wins, and a config's request needs [trust](permissions.md#config-trust)
- **Materialized, then verified on load** — every pinned file lands in `.goccia/packages/…` from `raw.githubusercontent.com` (GET only, host-pinned, private ranges refused), and each file's bytes are hashed again when a module, data file, or native library is loaded
- **Not part of the module graph by location** — `.goccia` is excluded from the [module-graph exemption](permissions.md#the-module-graph-exemption); package files load only as a resolved package, and a package imports only its own files
- **Offline by default once cached** — a complete cache needs no network, and `--cached-only` turns a missing or changed file into an error instead of a fetch

The decision is recorded in
[ADR 0122](adr/0122-unified-capability-model.md). The capability grammar is in
[Permissions](permissions.md#import-scopes); this page is the reference for
the import map, the lockfile, and the cache.

## The import map

A provider entry sits in the `imports` object of `goccia.json` (or an
`--import-map` file) beside ordinary entries:

```json
{
  "imports": {
    "@/": "./src/",
    "raylib": "github:frostney/GocciaScript-Raylib@v0.10.0/bindings/raylib.ts",
    "raylib/": "github:frostney/GocciaScript-Raylib@v0.10.0/bindings/"
  }
}
```

The address is `github:<owner>/<repo>@<ref>[/<path>]`:

| Part | Rule |
|---|---|
| `<owner>`, `<repo>` | GitHub names: letters, digits, `-` (and `_`, `.` in a repository). Compared case-insensitively. |
| `<ref>` | A tag, or a 40-character lowercase commit. Letters, digits, `.`, `_`, `-`, `+`; no `/`, so the path starts at the first `/`. Branches are not accepted: nothing can later prove which commit a moved branch named. |
| `<path>` | Empty for the repository root, a file for an exact entry, or a directory ending in `/` for a prefix entry. A relative path with no `..`, `.`, or empty segment. |

An exact entry resolves to its file with the usual
[extension probe](module-resolution.md#resolution-order): the path, its
TypeScript sources, each extension, then `<path>/index.<ext>`. An entry with no
path resolves the package's `index` file. A prefix entry appends the rest of
the specifier to its directory, so `import "raylib/lib/structs"` names
`bindings/lib/structs` in the package; a tail that climbs out of the directory
(`raylib/../x`) is refused. Only files the lockfile pins are candidates:
anything else in the cache does not exist for resolution.

A provider entry is recorded when the import map loads and resolved when an
import first goes through it, so a run that never imports the package needs no
grant for it.

## The lockfile

`goccia.lock.json` sits beside the file that declares the entry. It pins each
package, keyed by `github:<owner>/<repo>@<ref>` exactly as the import map
writes it:

```json
{
  "version": 1,
  "packages": {
    "github:frostney/GocciaScript-Raylib@v0.10.0": {
      "ref": "tag",
      "commit": "3f9c2a1e5b7d0c4a8e6f1b2d3c4e5f6a7b8c9d0e",
      "artifacts": {
        "bindings/raylib.ts": { "sha256": "0b46f4d0…" },
        "native/linux-x86_64/libraylib.so": { "sha256": "1d2f…" },
        "native/windows-x86_64/raylib.dll": { "sha256": "948e…" }
      }
    }
  }
}
```

- `ref` is `tag` or `commit`. A commit pin's key names that commit.
- `commit` is the 40-character lowercase commit every file is fetched from.
- `artifacts` lists every file of the package with its SHA-256 as 64
  lowercase hexadecimal digits. Every platform's native libraries are pinned
  and materialized; there is no platform field.

The reader is strict. An unknown or duplicate key, a value of the wrong type,
a version other than 1, an unsafe path (absolute, `..`, a backslash or colon,
a Windows device name, a `goccia.*` file, a `.goccia` directory), two paths
that differ only in case, or a path that is both a file and a directory fails
the import. The lockfile pins no URL: each file's URL is derived as
`https://raw.githubusercontent.com/<owner>/<repo>/<commit>/<path>`, so neither
the import map nor the lockfile can point a run at another host.

A run never resolves a ref and never writes the lockfile. A package the import
map names but the lockfile does not pin fails with
`<package> is not pinned in goccia.lock.json`.

## Authorization

Resolving through a provider entry needs the `import` capability:

```sh
GocciaRunner app.ts --allow-import=github:frostney/GocciaScript-Raylib
GocciaRunner app.ts --allow-import=github:frostney        # every repository of frostney
GocciaRunner app.ts --allow-import=github --deny-import=github:evil
```

Without a covering scope the import throws `PermissionDenied` with the message
`import: <package key>` and a host-side suggestion naming the scopes that would
grant it, or the deny that refused it. The grant is checked before the
lockfile is read or anything is fetched. The same scopes work in a config's
`permissions` block, where `allow-import` is a request that needs trust. There
is no separate network grant: the provider's hosts are fixed by the
implementation, and `net` scopes are never consulted. Sandbox mode grants no
`import` capability, so a provider import there is refused.

Importing never implies FFI. A package that ships native libraries still needs
`--allow-ffi` for the cache path, such as
`--allow-ffi=.goccia/packages/github/frostney`.

## The cache

Packages materialize under the import map's directory:

```text
.goccia/packages/github/<owner>/<repo>/<commit>/<path>
```

Owner and repository are lowercased. On first resolution every pinned file of
the package is checked. A cached file whose SHA-256 matches is reused; a
missing or mismatching one is fetched from its derived URL:

- one GET per file, with no body and no credentials, to
  `raw.githubusercontent.com` on port 443 only — every redirect hop to another
  host or port is refused before it is sent;
- private, loopback, and link-local destinations refused;
- a 60-second deadline and a 64 MiB ceiling per file;
- anything but HTTP 200, or bytes that do not match the pin, fail the import
  and are never written.

A verified file is written to a temporary created exclusively beside it and
renamed into place. The walk from `.goccia` down opens each directory without
following a symbolic link, and a link anywhere below `.goccia` — a directory
or the file itself, even one pointing at the pinned bytes — fails the import.
Commit `goccia.lock.json`; add `.goccia/` to `.gitignore`.

`--cached-only` (or `"cached-only": true` in config) refuses the network: a
missing file fails with `<package> is not cached (<path>)`, and a file that
does not match its pin with `the cached <path> does not match its pin`,
without a fetch. CI can restore `.goccia/packages` keyed on the lockfile's
hash and run with `--cached-only`.

## Verify on load

The materialization check is not trusted at load time. Each module, `json`,
`text`, or `bytes` import, and native library read from a materialized package
is hashed from the exact bytes about to be compiled, read, or loaded, and
refused unless they match the pin:

```text
Provider package file github:frostney/raylib@v1.0.0/bindings/late.ts changed after it was verified
```

A file inside the package that the lockfile does not pin is refused the same
way. `FFI.open` hashes a package library through the descriptor it loads on
Linux and the pinned handle on Windows; on macOS and other Unix systems the
path is hashed just before the load, which leaves the window described in
[Permissions](permissions.md#read-and-ffi-paths).

## Package files

The `.goccia` directory is not covered by the module-graph exemption. A
package's files become part of the module graph only as a resolved package,
and inside it a literal relative import that stays within the package root
needs no read grant. `import "./.goccia/packages/…"` from project code is an
ordinary read that needs `--allow-read`, and gets no pin check unless the
package was materialized in the same run.

A package imports only its own files, by relative specifier. A bare, absolute,
or `github:` specifier, or a relative one that leaves the package root, fails
with `Provider package <key> cannot import "<specifier>"`. There are no
transitive provider dependencies. Package files run with the project's
capabilities and configuration, like any project file.

`FFI.open` accepts a `file:` URL, as a `URL` object or a string, and judges it
as the path it names, so a package can open a library beside itself:

```javascript
const lib = FFI.open(new URL("../native/linux-x86_64/libraylib.so", import.meta.url));
```

## Audit events

Every decision emits an `import.provider`
[capability audit event](capability-audit.md):

| Subject | Reasons |
|---|---|
| `github:<owner>/<repo>@<ref>` | `the import capability covers github:<owner>/<repo>`, `the import capability does not cover …`, `refused by the import deny …` |
| a derived file URL | `sha256 ok`, `sha256 mismatch`, `HTTP <status>`, or a transport error |
| `<package key>/<path>` | `the cached file matches its pin`, `the cached file does not match its pin`, `the loaded bytes match the pin`, `the loaded bytes do not match the pin`, `the file is not pinned in goccia.lock.json` |

## Related documents

- [Permissions](permissions.md) — the capability grammar, config trust, and the module-graph exemption
- [Module Resolution](module-resolution.md) — the resolution order provider entries join
- [Capability Audit Events](capability-audit.md) — the event contract
- [FFI](built-ins-ffi.md) — opening native libraries
- [ADR 0122](adr/0122-unified-capability-model.md) — the decision
