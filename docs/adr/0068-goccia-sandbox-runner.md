# 0068 - GocciaSandboxRunner with explicit seed baselines

## Status

Accepted.

## Context

GocciaScript needs a first-class way to run JavaScript with a virtual filesystem while preserving the project's sandbox-first runtime boundary. The ordinary `GocciaScriptLoader` runs host files or stdin and exposes the loader runtime profile. Widening that loader with ambient filesystem behavior would blur the difference between host files and sandbox files, and would make it too easy for scripts to accidentally depend on the caller's filesystem.

The sandbox runner also needs to work with both execution modes. The engine already has interpreter and bytecode executors behind a shared `TGocciaExecutor` contract, so sandboxing must sit at the CLI host and runtime-extension layer rather than inside one executor.

Seed data is the central design constraint. A sandbox cannot import the whole host filesystem, and a host path must not become a live mount. The runner needs explicit ways to populate files and folders before execution, define inline text or binary files, run nested scripts in the same sandbox, and report changes without implicitly writing back to the host.

## Decision

Add `GocciaSandboxRunner` as a separate CLI host.

The runner owns a `TSandboxVirtualFileSystem`, imports seed baselines before execution, captures that filesystem as the diff baseline, and executes a sandbox entry path such as `/main.js`. The entry path, all module imports, and all runtime filesystem operations resolve inside the sandbox filesystem.

The runner supports the same execution-mode option as other execution tools:

- `--mode=interpreted` runs through `TGocciaInterpreterExecutor`.
- `--mode=bytecode` runs through `TGocciaBytecodeExecutor`.

Seed population is explicit:

- `--seed=<host>[=<sandbox>]` imports a host path relative to the invocation current working directory.
- `--seed-config=<file.json>` loads a JSON seed configuration.
- Seed config `from` paths are relative to the seed config file.
- Seed config inline text entries use `{ "path": "/file.txt", "text": "utf-8 text" }`.
- Seed config inline binary entries use `{ "path": "/file.bin", "base64": "..." }`.

Seed paths are import baselines, not mounts. A seeded host directory is copied into the virtual filesystem before execution. Later host changes are not visible to the sandbox, and sandbox writes do not write back to the host path.

The runner installs sandbox capabilities as import-only modules:

- `"fs"` exposes Node.js-shaped synchronous, callback, and `fs.promises` APIs, backed by the sandbox filesystem.
- `"goccia"` exposes `$` and `runScript`.

No global `fs`, `$`, or `runScript` bindings are added. This keeps capability use visible at the import site and avoids changing the ordinary loader runtime surface.

`$` is a Bun-like shell command factory. It accepts tagged templates or command strings, returns a lazy command object, and supports `.run()`, `.text()`, `.json()`, `.quiet()`, and `.nothrow()`. Shell commands operate on the sandbox filesystem. The shell also supports a `goccia <sandbox-entry.js>` builtin for nested execution.

`runScript(path)` executes another sandbox entry path using the same virtual filesystem and the same execution mode. It returns a structured result with stdout, stderr, exit code, result, and error information.

`fetch` remains governed by the existing allowed-host model. The sandbox runner applies the loader runtime profile, so `fetch` exists, but requests require `--allowed-host` or the equivalent config setting just like the script loader.

Filesystem export is not implicit. The runner supports explicit diff output:

- `--diff` prints the diff after execution.
- `--diff-output=<host-path>` writes the diff to a host file.
- `--diff-format=json|unified` selects the diff format; JSON is the default.

## Consequences

Sandboxed execution has a separate executable and a separate runtime extension, which keeps `GocciaScriptLoader` focused on host-file execution and avoids ambient filesystem access in the default loader path.

The virtual filesystem becomes the single source of truth for sandbox modules and filesystem operations. Module resolution needs a sandbox resolver and content provider so imports never fall back to host paths.

Seed configuration is intentionally JSON-only for the sandbox population format. General Goccia config remains the existing CLI/config-file system; seed config describes initial filesystem content.

Diffs are consumer-facing artifacts rather than write-back behavior. Tools that want to materialize sandbox changes must consume the diff and choose how to apply it.

The `"fs"` module is Node.js-shaped but not Node.js-compatible in full. It is a sandbox filesystem API with a familiar naming scheme; unsupported Node-specific features should remain absent unless they map cleanly to the virtual filesystem.

Callback and Promise filesystem operations enqueue a GC-rooted executable job
in the engine microtask queue. The operation itself runs when that job is
drained, so the virtual filesystem does not mutate before the asynchronous API
returns. This is a sandbox scheduling approximation, not an implementation of
Node's libuv filesystem phase ordering.

Nested execution shares filesystem state by design. This makes `runScript` and shell `goccia` useful for orchestrating sandboxed workflows, while still avoiding any host mount semantics.
