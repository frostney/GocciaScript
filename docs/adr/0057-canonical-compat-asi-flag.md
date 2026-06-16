# Canonical compat-asi flag

**Date:** 2026-05-24
**Area:** `parser`

ASI now uses the compatibility flag surface as `--compat-asi` / `"compat-asi"` with no legacy alias. Goccia-specific CLI option metadata lives in `Goccia.CLI.Options`, where compatibility flags are table-driven and resolved into the source-pipeline compatibility set; embedders use `TGocciaEngine.Compatibility` rather than per-flag convenience properties. [language.md § Automatic Semicolon Insertion](../language.md#automatic-semicolon-insertion). [embedding.md § Preprocessors and Compatibility](../embedding.md#preprocessors-and-compatibility).
