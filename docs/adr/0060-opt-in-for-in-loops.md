# Opt-in for-in loops

**Date:** 2026-06-02
**Area:** `engine`

Added opt-in `for...in` JavaScript compatibility behind `--compat-for-in-loop` / `"compat-for-in-loop"` / `cfForIn`. The flag remains separate from `--compat-traditional-for-loop` because `for...in` uses property-name enumeration rather than counted-loop control flow. It stays off by default with the existing parser warning/no-op behavior and supports enumerable string own and inherited keys in interpreter and bytecode modes. [language.md](../language.md#forin-loop).
