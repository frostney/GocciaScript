# Remove compat-all flag

**Date:** 2026-05-10
**Area:** `engine`

Removed the `--compat-all` meta-flag. It was originally added to give test262 a single switch that turns on every `--compat-*` option, but in practice it (a) hides which compatibility surface a given run actually depends on, (b) silently widens its own meaning whenever a new `--compat-*` flag lands, and (c) needs the same fan-out logic kept in sync in three frontends (`GocciaScriptLoaderBare.dpr`, `Goccia.CLI.Application.pas`, `GocciaBundler.dpr`). Consumers now enumerate the specific flags they need; `scripts/run_test262_suite.ts` lists the required `--compat-*` flags explicitly for the conformance corpus. The individual compat flags themselves and their `goccia.json` keys are unchanged.
