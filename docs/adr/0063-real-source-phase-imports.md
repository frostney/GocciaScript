# Real source-phase imports

**Date:** 2026-06-10
**Area:** `engine`

Source-phase imports now use a real JavaScript module source provider instead of only recognising the test262 `<module source>` sentinel. `LoadModuleSourceValue` resolves through the normal module resolver, parses JavaScript module source for early errors without linking or evaluating it, caches a `ModuleSource` object by resolved source-phase request key, and reports non-JavaScript module kinds as having no source representation. [language.md](../language.md#modules).
