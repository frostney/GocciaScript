# Split engine and runtime entry points

**Date:** 2026-05-02
**Area:** `runtime`
**Pull Request:** [#493](https://github.com/frostney/GocciaScript/pull/493)

Engine and runtime entry points are split explicitly. `TGocciaEngine` owns language execution, core built-ins, source strings/source lists, and Script-vs-Module entry semantics through `SourceType`; `TGocciaRuntime` owns host/runtime globals, special-purpose opt-ins such as test assertions/benchmarks/FFI, file-backed convenience helpers, and the default filesystem module content provider. `GocciaScriptLoaderBare` remains a core-engine-only frontend that can read file/stdin into source text without attaching runtime globals, and the CLI CI suite now covers that contract. [architecture.md § Main Layers](../architecture.md#main-layers). [embedding.md § Engine API](../embedding.md#engine-api).
