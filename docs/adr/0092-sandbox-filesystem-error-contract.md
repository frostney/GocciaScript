# Convert sandbox filesystem failures at the runtime seam

**Date:** 2026-07-13
**Area:** `sandbox`, `runtime`, `errors`
**Issue:** [#869](https://github.com/frostney/GocciaScript/issues/869)
**Related:** [#824](https://github.com/frostney/GocciaScript/issues/824), [#868](https://github.com/frostney/GocciaScript/issues/868), [ADR 0068](0068-goccia-sandbox-runner.md)

Sandbox virtual filesystem exceptions remain typed Pascal exceptions with no
dependency on GocciaScript values. The sandbox runtime converts them through a
dedicated `Goccia.Sandbox.FileSystemErrors` module before they cross into an
executor. This keeps the virtual filesystem reusable while ensuring the
interpreter and bytecode VM receive the same JavaScript throw value.

The converted value is a real JavaScript `Error` with Node-shaped system-error
metadata. It always has `code`, `errno`, `path`, and `syscall`; two-path
operations also have `dest`. Messages use the form
`CODE: description, operation 'path'`, with `-> 'dest'` appended for rename and
copy operations.

`syscall` names the stable public sandbox operation (`readFile`, `writeFile`,
`appendFile`, `mkdir`, `readdir`, `stat`, `exists`, `rm`, `rename`, or
`copyFile`) rather than an operating-system syscall. The name is identical for
synchronous, promise, and callback forms. This is deliberate because the
in-memory filesystem performs no host filesystem call and because all API forms
must share one observable error contract.

`code` is portable. `errno` is the target-appropriate negative libuv-style
number: Unix targets use the native errno number with a negative sign, while
Windows uses libuv's normalized values. Sandbox quota exhaustion maps to
`ENOSPC`, the closest portable libuv analogue. An unclassified base filesystem
exception maps defensively to `EIO`.

Consequences:

- Sync methods throw the converted `Error`; promise methods reject with that
  exact value; callback methods can reuse it when #868 installs the callback
  surface.
- The public string code is stable across targets, while consumers must not
  assume that numeric `errno` is cross-platform.
- This contract is intentionally limited to the sandbox `fs` runtime module and
  does not make GocciaScript a general Node.js host environment.
