# Annex B labelled functions

**Date:** 2026-06-07
**Area:** `parser`

Labelled function declarations now follow Annex B compatibility boundaries. Direct sloppy labelled functions remain available behind `--compat-label` plus `--compat-function`, immediate labelled-function bodies under `if`/`else`/`with`/iteration statements are rejected per Annex B.3.1, and sloppy block/switch function declarations update the nearest var binding only when `--compat-non-strict-mode` is also active. [language.md § function Keyword](../language.md#function-keyword). [language.md § Labeled Statements](../language.md#labeled-statements).
