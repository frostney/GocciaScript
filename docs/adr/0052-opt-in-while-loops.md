# Opt-in while loops

**Date:** 2026-05-17
**Area:** `engine`
**Pull Request:** [#651](https://github.com/frostney/GocciaScript/pull/651)

Added opt-in `while` and `do...while` JavaScript compatibility behind `--compat-while-loops` / `"compat-while-loops"` in interpreter and bytecode modes. The flag is separate from `--compat-traditional-for-loop`, stays off by default with the existing parser warning/no-op behavior, and supports condition testing, `break`, `continue`, `return`, timeout checks, instruction limits, and module/config propagation when enabled. [language.md](../language.md#while-and-dowhile).
