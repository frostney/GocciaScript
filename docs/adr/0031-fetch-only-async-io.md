# Fetch-only async I/O

**Date:** 2026-04-24
**Area:** `runtime`

Fetch-only asynchronous I/O. `fetch()` now returns a pending Promise before network completion and runs blocking HTTP work on fetch-specific background workers. Completions are settled back on the owning runtime thread, then normal Promise reactions drain through the existing microtask queue; the microtask queue remains a Promise-reaction queue, not an I/O queue. `await fetch(...)` deliberately keeps the current synchronous await model by pumping fetch completions while it waits; a general event loop and true async continuations remain future work. [built-ins.md § fetch](../built-ins.md#fetch-gocciabuiltinsglobalfetchpas).
