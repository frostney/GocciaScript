# 0189 - Windows builds carry ICU's own libraries

**Date:** 2026-11-24
**Area:** `intl`, `build`, `release`
**Related:** [ADR 0188](0188-the-delphi-projects-and-the-wasm-test-runner-leave-with-the-old-engine.md)

## Context

`Intl` and Temporal take their data from ICU. Linux loads the distribution's
`libicui18n` and macOS the system's `libicucore`; both export ICU's full C
API. Windows' own `icu.dll` exports only part of it. Without
`udtitvfmt_formatCalendarToResult`, `Intl.DateTimeFormat.prototype.formatRange`
cannot format a range with the proleptic Gregorian calendar, so dates before
1582 came out in the Julian calendar on Windows alone, and two tests failed
there.

## Decision

Windows builds carry ICU's own release, 79.1, beside their executables:
`icuin79.dll`, `icuuc79.dll` and `icudt79.dll`, with ICU's license as
`ICU-LICENSE.txt`. `scripts/fetch-windows-icu.sh` downloads the release from
unicode-org/icu and checks it against a pinned SHA-256; CI's build job puts
it into each Windows artifact, so the test, CLI and benchmark jobs and the
release archives get it.

`source/shared/ICU.pas` loads the newest `icuin<major>.dll` (major 70 or
later) found beside the executable or on `PATH`, with its `icuuc` and the
`_<major>` symbol suffix ICU's build uses, and falls back to the system's
`icu.dll`.

## Consequences

- Windows behaves as the other platforms do, and its test runs pass in full.
- Windows release archives grow by about 40 MB, most of it `icudt79.dll`.
- ICU's DLLs need the Visual C++ 2015-2022 runtime (`vcruntime140.dll`,
  `msvcp140.dll`); without it, they do not load and the system `icu.dll` is
  used.
- A newer ICU is a change to the script's version and hashes. ICU 79 drops
  names some older data had, such as CNX's; `Intl.supportedValuesOf("currency")`
  follows the loaded data.
