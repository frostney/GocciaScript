# Sandbox Built-ins

*For script authors using `GocciaSandboxRunner` and contributors extending its sandbox runtime surface.*

## Executive Summary

- **Import-only capabilities** — Sandbox filesystem and shell helpers are provided by `"fs"` and `"goccia"` modules, not globals
- **Virtual filesystem only** — `fs` reads and writes the sandbox VFS; it never reaches the host filesystem
- **Shared by default** — `runScript(path)` and shell `goccia path` execute inside the current sandbox filesystem unless child isolation is requested
- **Child sandbox option** — `runScript(path, { sandbox: true, seed: [...] })` and `goccia --sandbox --seed ... path` spin up a nested VFS seeded from the parent VFS
- **Diffs are explicit** — Child sandbox diffs are returned only when requested

`GocciaSandboxRunner` installs sandbox capabilities as import-only modules. They are not globals, and they are not loaded from the virtual filesystem.

```javascript
import fs from "fs";
import { $, runScript } from "goccia";
```

## `fs`

The `"fs"` module operates on the sandbox virtual filesystem. It follows a Node.js-shaped subset while keeping every path inside the sandbox namespace.

| Method / Property | Description |
|-------------------|-------------|
| `fs.readFileSync(path, options?)` | Read text when `options` is `"utf8"` / `"utf-8"` or `{ encoding: "utf8" }`; otherwise returns `Uint8Array` |
| `fs.writeFileSync(path, data)` | Write UTF-8 string data or bytes from `Uint8Array` |
| `fs.appendFileSync(path, data)` | Append UTF-8 string data or bytes from `Uint8Array` |
| `fs.mkdirSync(path, options?)` | Create a directory; `{ recursive: true }` creates parents |
| `fs.readdirSync(path)` | Return child names |
| `fs.statSync(path)` | Return `{ path, name, type, size, mtimeMs, isFile(), isDirectory() }` |
| `fs.existsSync(path)` | Return whether a sandbox path exists |
| `fs.rmSync(path, options?)` | Delete a file or directory; `{ recursive: true }` removes non-empty directories |
| `fs.renameSync(from, to)` | Move or rename a sandbox path |
| `fs.copyFileSync(from, to)` | Copy a file |
| `fs.promises` | Promise-returning versions of the same operations |

`readFileSync` returns a `Uint8Array` by default so binary seed entries can round-trip without text coercion.

## `goccia`

The `"goccia"` module exposes sandbox runner orchestration helpers.

| Export | Description |
|--------|-------------|
| `$` | Bun-like shell tagged template / command factory. Commands run against the sandbox filesystem and return a lazy command object with `.run()`, `.text()`, `.json()`, `.quiet()`, and `.nothrow()` |
| `runScript(path, options?)` | Execute another sandbox entry path with the same execution mode. Shared VFS is the default; `{ sandbox: true }` creates a child VFS. Returns `{ ok, exitCode, stdout, stderr, result, error, diff }` |

`$` accepts tagged templates or command strings. Tagged-template substitutions are shell-quoted before command parsing.

```javascript
const name = "hello world";
console.log(await $`echo ${name}`.text());
```

By default, nested execution shares the current virtual filesystem:

```javascript
const child = runScript("/child.js");
const viaShell = await $`goccia /child.js`.text();
```

Pass `{ sandbox: true }` to run the child in a fresh virtual filesystem. Seed entries are sourced from the current sandbox VFS, not from the host:

```javascript
const child = runScript("/child.js", {
  sandbox: true,
  seed: [
    "/child.js",
    { from: "/lib", to: "/lib" },
    { path: "/input.txt", text: "inline text" },
    { path: "/data.bin", base64: "AQID" },
  ],
  diff: true,
  diffFormat: "json",
});

console.log(child.stdout);
console.log(child.diff);
```

`seed` accepts one entry or an array. A string entry copies that parent-VFS path to the same child path. `{ from, to }` copies a parent file or directory into a child target path. When a file seed target ends in `/`, the file is copied under that directory with its source name. Inline `{ path, text }` and `{ path, base64 }` entries create child-only files.

The sandbox shell also exposes the same child mode:

```javascript
const out = await $`goccia --sandbox --seed /child.js --seed /lib=/lib --diff /child.js`.text();
```

Shell `goccia` supports `--sandbox`, repeatable `--seed <from[=to]>` / `--seed=<from[=to]>`, `--diff`, and `--diff-format json|unified`. Child diffs are appended to command stdout only when `--diff` is present.
