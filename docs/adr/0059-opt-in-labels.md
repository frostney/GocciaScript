# Opt-in labels

**Date:** 2026-05-29
**Area:** `engine`

Labeled `break` and `continue` targets are available behind `--compat-label` / `"compat-label"` for JavaScript compatibility and test262 coverage. Labels stay disabled by default, but when enabled the parser preserves label targets and both interpreter and bytecode paths resolve named breaks and iteration-only continues. [language.md § Labeled Statements](../language.md#labeled-statements).
