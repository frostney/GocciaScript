# Centralized parser policy

**Date:** 2026-05-24
**Area:** `parser`

Parser policy now lives behind the source pipeline instead of being repeated at host call sites. `TGocciaSourcePipeline` owns compatibility/source-type mapping and exposes narrow parse entry points for full source, module source, dynamic `Function` validation/wrapping, and expression fragments; module loading consumes a module-specific result rather than the full host-facing parse report. [architecture.md § Overview](../architecture.md#overview). [embedding.md § Engine API](../embedding.md#engine-api).
