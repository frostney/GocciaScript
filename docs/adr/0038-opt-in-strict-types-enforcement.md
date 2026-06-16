# Opt-in strict-types enforcement

**Date:** 2026-04-30
**Area:** `runtime`

Strict-types runtime enforcement is now an explicit `--strict-types` CLI / `"strict-types"` config flag (default off) that works in both interpreter and bytecode mode. Previously the bytecode VM implicitly enforced type annotations and the interpreter silently ignored them, with a read-only `Goccia.strictTypes` JS global advertising the mode. The new model: enforcement is opt-in regardless of execution mode, the bytecode compiler gates `OP_CHECK_TYPE` emission on the flag, the interpreter records type hints on lexical bindings (`TLexicalBinding.TypeHint`) and enforces them in `AssignBinding` / variable declaration / parameter binding via the shared `Goccia.Types.Enforcement` unit, and the `Goccia.strictTypes` global is removed in favour of directory-level `goccia.json` config (mirroring `--asi` / `--compat-var`). [language.md § Types as Comments](../language.md#types-as-comments-stage-1).
