# Sandbox Built-ins

*For script authors using `GocciaSandboxRunner` and contributors extending its sandbox runtime surface.*

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
| `runScript(path)` | Execute another sandbox entry path with the same virtual filesystem and execution mode. Returns `{ ok, exitCode, stdout, stderr, result, error }` |

`$` accepts tagged templates or command strings. Tagged-template substitutions are shell-quoted before command parsing.

```javascript
const name = "hello world";
console.log(await $`echo ${name}`.text());
```

The sandbox shell also exposes a `goccia <sandbox-entry.js>` builtin, equivalent to nested script execution from inside `$`.

```javascript
const child = runScript("/child.js");
const viaShell = await $`goccia /child.js`.text();
```
