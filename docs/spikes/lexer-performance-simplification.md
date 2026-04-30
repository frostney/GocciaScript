# Lexer Performance Simplification

Point-in-time spike for reducing lexer hot-path overhead and code size

## Executive Summary

- Direct cursor advancement is faster than routing every skipped comment character through `Advance`.
- Regex literal scanning should collect the pattern with one `Copy` after scanning, not append each pattern byte into a `TStringBuffer`.
- Small lexer primitives are reasonable `inline` candidates when measured, especially `Advance` and line-terminator helpers.
- The numeric separator helper was rejected, even with `inline`, because it shortened the code but regressed decimal and radix-heavy cases.
- The final lexer change reduced `Goccia.Lexer.pas` from 1744 to 1632 lines while keeping the full test suite green.

## Context

The lexer had several hot paths where simple scanning work went through helper calls or repeated equivalent branches:

- hashbang and single-line comments called `IsLineTerminator` and `Advance` for each skipped character
- block comments repeatedly called `Peek` and `Advance`
- template interpolation scanning duplicated line-terminator and "append current char to cooked/raw buffers" logic
- regex literal scanning appended each pattern byte into `TStringBuffer`
- numeric separator scanning repeated nearly identical loops for decimal, radix, fraction, and exponent parts

The goal was to simplify the code without losing lexer correctness, and to keep changes only when the integrated timing tools showed neutral or better behavior.

## Methodology

Measurements used the repository's benchmark runner and its JSON timing output:

```bash
./build.pas --prod benchmarkrunner
GOCCIA_BENCH_CALIBRATION_MS=1 \
GOCCIA_BENCH_ROUNDS=1 \
GOCCIA_BENCH_WARMUP=1 \
  ./build/GocciaBenchmarkRunner --mode=bytecode --format=json --no-progress
```

The measured field was `files[0].lexTimeNanoseconds`, converted to milliseconds. Runs alternated before/after binaries to reduce order bias. The final before/after comparison used a detached `HEAD` worktree as the baseline and the working tree as the candidate. Values below are medians.

Environment for the final run:

- Date: 2026-04-29
- FPC: 3.2.2
- OS/arch: Darwin aarch64
- Build: production benchmark runner
- Baseline commit: `ba88a512`

These are synthetic lexer stress cases, not a replacement for the full benchmark suite.

## Accepted Changes

### Direct comment scanning

`SkipUntilLineTerminator` advances `FCurrent` and `FColumn` directly until it sees LF, CR, LS, or PS. Hashbang and single-line comments use this path.

Block comments now scan `FSource[FCurrent]` directly and only delegate to the shared line-terminator helper when a terminator is actually present. This removes repeated `Peek`/`Advance` calls from the common non-terminator path.

### Shared line-terminator handling

`ConsumeLineTerminator` centralizes line and column updates for LF, CR, CRLF, LS, and PS. `AppendLineTerminator` uses the same tracking while appending the correct cooked/raw template bytes.

This removed duplicated CR/LF/Unicode branches from whitespace and template interpolation code.

### Template append helper

`AppendCurrent` consumes the current source byte and appends it to both template buffers. This made nested template and interpolation scanning smaller without changing how raw and cooked content are collected.

### Regex pattern slicing

Regex literal scanning now records `PatternStart`, scans through the pattern, and slices the source once:

```pascal
Pattern := Copy(FSource, PatternStart, FCurrent - PatternStart - 1);
```

This avoids one `TStringBuffer.AppendChar` per regex pattern byte.

### Selected inline hints

The final inline pass kept only small primitives that measured neutral or faster:

- `Advance`
- `IsValidIdentifierChar`
- `ConsumeUnicodeLineTerminator`
- implementation-side `inline` markers for small methods already declared inline

`inline` remains a compiler hint, not a guarantee. The decision was based on measured behavior from the production benchmark runner.

## Rejected Changes

### Numeric separator helper

A helper for digit runs with numeric separators shortened `ScanNumber`, but it was rejected. The first non-inline spike regressed the mixed operator/number case by about 6%. Adding `inline` helped one mixed case but still regressed decimal-heavy and radix-heavy inputs.

Inline helper spike, measured against the current non-helper implementation:

| Case | Delta |
|---|---:|
| `operators_numbers` | -5.4% |
| `decimal_numbers` | +15.6% |
| `base_numbers` | +7.3% |
| `line_comments` | +11.0% |
| `templates_interp` | +6.4% |

The duplicated loops in `ScanNumber` are deliberately kept because they are clearer to the compiler and faster across the important numeric cases.

## Final Before/After Results

Final current tree compared against baseline `HEAD` (`ba88a512`). Each row uses 18 alternating repeats except `templates_large`, which uses 30 repeats to reduce noise in the template case.

| Case | Before | After | Delta |
|---|---:|---:|---:|
| `operators_numbers` | 9.942 ms | 9.999 ms | +0.6% |
| `decimal_numbers` | 6.801 ms | 6.745 ms | -0.8% |
| `base_numbers` | 10.954 ms | 9.898 ms | -9.6% |
| `identifiers` | 14.948 ms | 14.160 ms | -5.3% |
| `regex_literals` | 2.790 ms | 2.397 ms | -14.1% |
| `line_comments` | 2.654 ms | 2.195 ms | -17.3% |
| `block_comments` | 2.469 ms | 1.713 ms | -30.6% |
| `templates_large` | 10.755 ms | 9.677 ms | -10.0% |

The mixed operator/number case is effectively neutral, while radix numbers, identifiers, regex literals, comments, and larger template interpolation inputs improved.

## Inline-Only Spike

The selected inline hints were measured separately against the pre-inline lexer changes:

| Case | Delta |
|---|---:|
| `operators_numbers` | -5.5% |
| `decimal_numbers` | -8.2% |
| `base_numbers` | -4.0% |
| `identifiers` | -0.4% |
| `regex_literals` | -11.2% |
| `line_comments` | -9.5% |
| `block_comments` | -3.2% |
| `templates_interp` | -2.6% |

This supports keeping tiny lexer primitives inline, while still avoiding larger helper extraction on hot numeric loops.

## Verification

Final verification after the lexer and docs work:

```bash
./format.pas --check
./build.pas clean tests
./build/Goccia.Lexer.Test
./build.pas clean testrunner
./build/GocciaTestRunner tests --asi --unsafe-ffi --no-progress
git diff --check
```
