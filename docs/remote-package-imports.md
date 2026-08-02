# Remote Package Imports

Provider-qualified package entries coexist with local entries in the same
`imports` object:

```json
{
  "remote-imports": true,
  "imports": {
    "@/": "./src/",
    "raylib": "github:frostney/GocciaScript-Raylib@v0.10.0"
  }
}
```

The same grant can be supplied explicitly on the command line:

```bash
./build/GocciaScriptLoader app.js --import-map=imports.json --remote-imports
```

The first provider slice supports only
`github:owner/repository@requested-ref`. The requested reference identifies
the package entry in a committed `goccia.lock.json` beside the import map; it
is never used directly for a download. The lockfile pins a lowercase
40-character Git commit plus SHA-256 hashes for every artifact:

```json
{
  "version": 1,
  "packages": {
    "github:frostney/GocciaScript-Raylib@v0.10.0": {
      "resolvedRef": "0123456789abcdef0123456789abcdef01234567",
      "entry": "bindings/raylib.ts",
      "artifacts": {
        "bindings/raylib.ts": {
          "sha256": "0b46f4d08b2f2c2a09d8b618703f48c4b658f9c1ea2c65f1ed9152096d9ebed5"
        },
        "native/libraylib.dylib": {
          "sha256": "948e9576967904e12729332b75d841cf195908a25cb4032b3238c1139e00225a",
          "platform": "darwin-aarch64"
        }
      }
    }
  }
}
```

Artifact paths are safe relative paths within the provider repository.
Artifacts without `platform` are universal; platform-specific artifacts use
`<build-os>-<build-arch>` such as `darwin-aarch64`, `linux-x86_64`, or
`windows-x86_64`. The package entry must be a universal or current-platform
artifact. Platform-native libraries are materialized alongside the package;
opening one still requires the independent FFI capability.

On first use, the resolver derives GET-only GitHub raw-content URLs from the
repository, pinned commit, and artifact paths. It verifies every response
before committing it under
`.goccia/packages/github/<owner>/<repository>/<commit>/`. Later runs verify
the cached bytes and perform no network request when they match, preserving
offline repeatability. A missing or corrupt cache entry may be replaced only
by a response matching the committed hash. Lockfile generation and updates
are explicit external project maintenance in this first slice; the runtime
never rewrites the lockfile or downloads in the background. Commit
`goccia.lock.json`; ignore the `.goccia/` cache directory.

`--remote-imports` is checked before any cache access, so disabling it rejects
the remote entry even when the cache is complete. It is separate from
`--allowed-host` and script-level `fetch`. Raw `http:`/`https:` addresses,
Node/npm resolution, and provider refs absent from the lockfile are rejected.
The host-backed package cache is not exposed inside `GocciaSandboxRunner`'s
virtual filesystem.
