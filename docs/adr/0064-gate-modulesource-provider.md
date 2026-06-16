# Gate ModuleSource provider

**Date:** 2026-06-10
**Area:** `engine`

JavaScript `ModuleSource` objects are now gated behind `--experimental-js-module-source` / `"experimental-js-module-source"` / `cfExperimentalJSModuleSource`. This keeps Stage 3 source-phase syntax enabled while requiring an explicit opt-in for the Stage 2.7 ESM Phase Imports JavaScript source-object provider; the test262 runner maps `source-phase-imports-module-source` tests to that flag instead of enabling it for all `source-phase-imports` tests. [language.md](../language.md#modules). [test262.md](../test262.md#source-phase-import-feature-flags).
