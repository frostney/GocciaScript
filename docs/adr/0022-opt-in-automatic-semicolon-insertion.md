# Opt-in automatic semicolon insertion

**Date:** 2026-04-08
**Area:** `parser`
**Pull Request:** [#220](https://github.com/frostney/GocciaScript/pull/220)

Opt-in automatic semicolon insertion. Added ASI behind a `--asi` CLI flag. Previously against the project philosophy (GocciaScript requires explicit semicolons), added for compatibility with code written for standard ECMAScript. Off by default; does not change the language subset, only relaxes the semicolon requirement. [language.md § Automatic Semicolon Insertion](../language.md#automatic-semicolon-insertion).
