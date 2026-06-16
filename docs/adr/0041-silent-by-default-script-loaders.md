# Silent-by-default script loaders

**Date:** 2026-05-04
**Area:** `runtime`
**Pull Request:** [#510](https://github.com/frostney/GocciaScript/pull/510)

Script loaders are silent-by-default about the script's last evaluated value, mirroring `node script.js` / `bun script.js` / `deno run script.js` / `qjs script.js`. Printing is opt-in via `--print` (or `"print": true` in `goccia.json` for `GocciaScriptLoader`), which emits the bare value on its own line including `undefined` — the same semantics as `node -p` / `bun --print` / `deno eval -p`. Replaces the previous "always print `Result: <value>` banner" behavior, which was the outlier among mainstream JS runtimes and surfaced as a false-positive vector in substring-graded environments such as test262.fyi (#509). `GocciaScriptLoader`'s human-readable timing banner ("Running script (interpreted): ..." + Lex/Parse/Execute line) is unchanged; only the final value line moved behind the flag. JSON output (`--output=json`) still always carries the result in the structured `result` field regardless of `--print`. [build-system.md § Compile and Run](../build-system.md#compile-and-run).
