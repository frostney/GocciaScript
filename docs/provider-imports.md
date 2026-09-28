# Provider Imports

*How an import-map entry naming a `github:` package becomes verified local files, what the `import` capability grants for it, and the rules a package's own files follow.*

## Executive Summary

- **Declared in the import map** — `"raylib": "github:<owner>/<repo>@<tag-or-commit>/<path>"` (an exact entry) or `"raylib/": "github:…@<ref>/<dir>/"` (a prefix entry) names a provider package; there is no separate package manifest
- **Pinned by `goccia.lock.json`** — the lockfile beside the import map pins each package to a commit and every file to a SHA-256; a run reads it and never writes it
- **Granted by `import` scopes** — `--allow-import=github`, `github:<owner>`, or `github:<owner>/<repo>`; a deny from any source wins, and a config's request needs [trust](permissions.md#config-trust)
- **Materialized, then verified on load** — every pinned file lands in `.goccia/packages/…` from `raw.githubusercontent.com` (GET only, host-pinned, private ranges refused), and each file's bytes are hashed again when a module, data file, or native library is loaded
- **Not part of the module graph by location** — `.goccia` is excluded from the [module-graph exemption](permissions.md#the-module-graph-exemption); package files load only as a resolved package, and a package imports only its own files
- **Offline by default once cached** — a complete cache needs no network, and `--cached-only` turns a missing or changed file into an error instead of a fetch
- **Pins come from install mode** — `GocciaRunner --add`, `--remove`, `--install`, and `--update` resolve tags and commits over `info/refs`, crawl each package's literal imports, and write the lockfile and then the import map; a run never does

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
| `<ref>` | A tag, or a 40-character lowercase commit. Letters, digits, `.`, `_`, `-`, `+`; no `/`, so the path starts at the first `/`, and a tag such as `release/1.0` cannot be pinned. Branches are not accepted: nothing can later prove which commit a moved branch named. |
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
grant for it. `import.meta.resolve` of a specifier an entry maps answers with
the provider address itself (`github:acme/lib@v1.0.0/index.js`), without a
grant, a fetch, or an audit event, as any resolve the engine would refuse
answers lexically.

## The lockfile

`goccia.lock.json` sits beside the file that declares the entry. It pins each
package, keyed by `github:<owner>/<repo>@<ref>` as the import map writes it.
A lookup compares owner and repository case-insensitively and the ref
exactly, so two keys that differ only in the case of a name pin one package
twice and are refused:

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
`<package> is not pinned in goccia.lock.json; run GocciaRunner --install`.

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
implementation, and `net` scopes are never consulted. The grant covers every
file resolution reaches through the package's entry, and every import a
package file makes of its own files, whether the specifier is literal or
computed: resolution there is confined to pinned files, each verified when it
loads, so no `read` grant is involved.

Sandbox mode resolves only an `--import-map` given on its command line, and
never the root config's import map; when that map has provider entries it
warns once, `<config> sets provider imports, which GocciaRunner sandbox mode
does not apply; ignoring them`. A provider entry of an explicit
`--import-map` is refused with `PermissionDenied`, since sandbox mode grants
no `import` capability.

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
`text`, or `bytes` import, data module (TOML, YAML, JSON5, CSV, TSV, JSONL),
and native library read from a materialized package is hashed from the exact
bytes about to be compiled, parsed, read, or loaded, and refused unless they
match the pin:

```text
Provider package file github:frostney/raylib@v1.0.0/bindings/late.ts changed after it was verified
```

A file inside the package that the lockfile does not pin is refused the same
way. `FFI.open` hashes a package library through the descriptor it loads on
Linux and the pinned handle on Windows, and from its path just before the
load on macOS and other Unix systems. The descriptor or handle fixes which
file is loaded, not its contents, so everywhere a library rewritten in place
between the hash and the load is not detected (see
[Permissions](permissions.md#read-and-ffi-paths)). A package's native library
must not load sibling libraries through `$ORIGIN` or `@loader_path`: on Linux
the library is loaded through `/proc/self/fd`, so such a lookup fails, and on
macOS a sibling would load without a pin check.

## Package files

The `.goccia` directory is not covered by the module-graph exemption. A
package's files become part of the module graph only as a resolved package,
and inside it a literal relative import that stays within the package root
needs no read grant. `import "./.goccia/packages/…"` from project code is an
ordinary read that needs `--allow-read`, and gets no pin check unless the
package was materialized in the same run.

A package imports only its own files, by relative specifier. A bare, absolute,
or `github:` specifier, or a relative one that leaves the package root, fails
with `Provider package <key> cannot import "<specifier>"`. No import-map
entry or alias applies to a package's imports, so a path key of the project's
map cannot carry one out of the package either. There are no transitive
provider dependencies. Package files run with the project's capabilities,
like any project file.

`FFI.open` accepts a `file:` URL, as a `URL` object or a string, and judges it
as the path it names, so a package can open a library beside itself:

```javascript
const lib = FFI.open(new URL("../native/linux-x86_64/libraylib.so", import.meta.url));
```

## Install mode

`GocciaRunner` maintains the pins in a mode of its own, like `--trust`: it
runs no code, takes no input files, and cannot be combined with sandbox mode
or `--cached-only` (exit 2).

```sh
GocciaRunner --add raylib=github:frostney/GocciaScript-Raylib@v0.10.0/bindings/raylib.ts
GocciaRunner --remove raylib
GocciaRunner --install --allow-import=github:frostney
GocciaRunner --install --frozen --check-refs --allow-import=github:frostney   # CI
GocciaRunner --update --allow-import=github:frostney
GocciaRunner --update=raylib --accept-moved-tags --allow-import=github:frostney
```

| Option | What it does |
|---|---|
| `--add <key>=github:<owner>/<repo>@<ref>[/<path>]` | Adds the entry, or replaces it (`Replace "<key>": <old> -> <new>`), and pins its package. Repeatable. The typed spec is the `import` grant for that package, for this invocation. |
| `--remove <key>` | Removes a provider entry; the package is re-pinned from its other entries, or dropped when none is left. Repeatable. |
| `--install` | Re-derives every pin from the import map at its locked commit, pins the entries the lockfile lacks, and fetches what the cache lacks. A locked ref is never re-resolved. |
| `--frozen` | With `--install`: a lockfile that would change is an error listing each package that changes (`+` added, `-` removed, `~` changed), and nothing is written. |
| `--check-refs` | With `--install`: each tag pin must still be its tag's commit. A commit pin that is no longer an advertised tip is a warning, since branches advance. |
| `--update[=<key>,…]` | Re-resolves the tags of every provider entry, or of the keys given. A commit pin cannot change and is not re-resolved. |
| `--accept-moved-tags` | With `--update`: re-pins a tag that now names another commit. |

The import map edited is the `goccia.json` found walking up from the working
directory, created by `--add` when there is none. Only its `imports` member
changes; every other byte stays, and so does the file's mode (narrowed by the
umask). An `--import-map` file, or a `goccia.json` that is a symbolic link, is
never edited: `--add` and `--remove` print the JSON line to change and write
nothing. Runs read imports only from `goccia.json` or `--import-map`, so a
`goccia.toml` or `goccia.json5` root config is a usage error in install mode.
An `--add`, `--remove`, or `--update` touches only the packages it names;
`--install` touches every package. A touched package is re-derived from all
of its entries, and a package no entry names any more is dropped and pruned.

**Refs.** A ref is resolved with one GET of
`https://github.com/<owner>/<repo>.git/info/refs?service=git-upload-pack`,
host-pinned and GET-only like every provider request. Only `refs/tags/*` and
`refs/heads/*` count; an annotated tag resolves to the commit it names. A tag
pins `ref: "tag"`. A 40-character commit pins `ref: "commit"`, and only when
it is the tip of an advertised tag or branch: the raw host serves a commit
that exists only in a fork under the upstream repository's name, and such a
commit is advertised only through `refs/pull/*`. A branch name, and an
abbreviated or uppercase commit, are refused.

The refs are listed again whenever a pin is about to be trusted: on every
`--add`, and whenever `--install` must fetch a package's files. A tag must
still name the locked commit, and a commit pin must still be an advertised
tip, so a lockfile edited to pin a fork's commit is caught before its bytes
are used. A fully cached `--install` needs no network. A tag that moved is an
error until `--update --accept-moved-tags` re-pins it.

**Pinned bytes.** Bytes fetched for a file the lockfile pins must match the
pin, since a commit's files cannot change: a mismatch is an error naming the
file, and neither the lockfile nor the cache is written. Only `--update`
moves a pin to another commit.

**The file set.** There is no package manifest. The crawl starts at each exact
entry's path (the `index` file for a directory or the repository root) and,
for a prefix entry, at every specifier the project's own modules import
through it. The project is scanned first: a package whose prefix entries
nothing imports yet is not pinned at all, needs no grant and no network, and
satisfies `--frozen`. The crawl follows literal `import` and `export … from`
specifiers and `import("…")` in modules, fetches `json`, `text`, and `bytes`
attribute imports as data, and fetches the literal first argument of
`new URL("./x", import.meta.url)` and `import.meta.resolve("./x")` as assets;
data and assets are pinned but never parsed. Computed specifiers are not
followed, so a package names every file it needs literally somewhere. A bare,
absolute, URL, or `github:` import inside a module, a path that leaves the
repository, and a `goccia.*` file are refused, and so are two files whose
paths differ only in case, before anything is written. Modules are tokenized
by the engine's parser with every compatibility flag on. A cached package is
crawled from its pinned files, the way a run resolves them. Each path is
fetched at most once, a missing candidate is asked for once, and a crawl stops
at 2,000 files, 256 MiB, or 10,000 requests.

**Order.** Every file is fetched and hashed in memory, and the new lockfile
checked, before anything is written. Then the cache is written. When entries
are added the lockfile is written before the import map; when entries are
removed the import map is written first. Either way a failure leaves at worst
a pin no entry uses, never an entry without a pin. Cache directories of pins
the new lockfile dropped are then deleted: every directory is opened without
following a symbolic link, and a link met is removed, never followed.

**Concurrency.** An install takes an exclusive operating-system lock on
`goccia.lock.json.lock` beside the lockfile (`flock` on POSIX, `LockFileEx`
on Windows), waits up to 10 seconds for another install to finish, and then
fails. The system releases the lock if the install dies; the file stays and
can be ignored like `.goccia/`.

The summary counts packages added, removed, and updated, and files added,
removed, and changed. Exit status is 0 on success; 1 for an out-of-date
lockfile under `--frozen`, a network or integrity failure, a moved tag, an
unadvertised commit, a deny, or a held lock; and 2 for a usage error (an
unknown `--remove` or `--update` key included) or an import map or lockfile
that is not valid (invalid JSON, an empty key, a malformed provider address).

## Audit events

Every decision emits an `import.provider`
[capability audit event](capability-audit.md):

| Subject | Reasons |
|---|---|
| `github:<owner>/<repo>@<ref>` | `the import capability covers github:<owner>/<repo>`, `the import capability does not cover …`, `refused by the import deny …` |
| a derived file URL | `sha256 ok`, `sha256 mismatch`, `HTTP <status>`, or a transport error |
| `<package key>/<path>` | `the cached file matches its pin`, `the cached file does not match its pin`, `the loaded bytes match the pin`, `the loaded bytes do not match the pin`, `the file is not pinned in goccia.lock.json` |

Install mode emits `import.provider` for each package's grant (`the --add
spec grants github:<owner>/<repo> for this invocation`, or the capability
that covers it) and for each `info/refs` and file request. It emits
`import.provider.install` with the package key as the subject and
`added: <ref kind> -> <commit>; <n> files`, `updated: <old> -> <new>; <n>
files added, <n> removed, <n> changed` (a file-set change at one commit
included), `removed; <n> files`, `refused by …`, `tag moved …`, or
`the commit is not the tip of an advertised tag or branch` as the reason.

## Related documents

- [Permissions](permissions.md) — the capability grammar, config trust, and the module-graph exemption
- [Module Resolution](module-resolution.md) — the resolution order provider entries join
- [Capability Audit Events](capability-audit.md) — the event contract
- [FFI](built-ins-ffi.md) — opening native libraries
- [ADR 0122](adr/0122-unified-capability-model.md) — the decision
