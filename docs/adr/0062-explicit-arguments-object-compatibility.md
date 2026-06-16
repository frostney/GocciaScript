# Explicit arguments object compatibility

**Date:** 2026-06-10
**Area:** `engine`

Removed the remaining implicit `--compat-non-strict-mode` activation of `arguments` objects and completed mapped arguments semantics behind the explicit `--compat-arguments-object` flag. The flag now controls whether the binding exists; source strictness and parameter-list shape control whether the object is mapped or unmapped, matching ES2026 §10.2.11 / §10.4.4.6 / §10.4.4.7. [language.md](../language.md#arguments-object). [interpreter.md](../interpreter.md#scope-chain-design). [bytecode-vm.md](../bytecode-vm.md#compatibility-scope-helpers).
