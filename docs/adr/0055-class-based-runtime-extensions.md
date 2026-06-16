# Class-based runtime extensions

**Date:** 2026-05-18
**Area:** `runtime`

Runtime host features are now class-based extensions installed onto `TGocciaRuntimeCore`, rather than a broad runtime-global selector. `GocciaScriptLoader` / `GocciaREPL` call `ApplyLoaderRuntimeProfile`; `GocciaTestRunner` layers `TGocciaTestingLibraryRuntimeExtension`; `GocciaBenchmarkRunner` layers `TGocciaBenchmarkRuntimeExtension`; FFI remains an explicit `--unsafe-ffi` extension. Feature-specific APIs such as fetch host restrictions and JSON5/TOML/YAML globals injection live on concrete extensions and are reached through runtime dispatch, keeping the base runtime extension generic and making smaller host surfaces easier to link. [architecture.md](../architecture.md#main-layers). [embedding.md](../embedding.md#runtime-extensions).
