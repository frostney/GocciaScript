# Parser Binary Expression Performance

Performance spike for reducing parser overhead in expression precedence loops.

## Executive Summary

- The parser benchmark runner already exposes `lex`, `parse`, and execution timing, so the spike used existing phase timing instead of adding a custom profiler.
- Replacing generic binary-expression loops with direct `Peek.TokenType` loops improved parser medians across large synthetic inputs by 3.6-18.9%.
- The same change improved existing benchmark-file parse medians by 6.3-14.3%, despite those files being small parser workloads.
- Other spikes were rejected: eager source-text removal and `Advance inline` regressed production parse medians, while repeated `Check(...)` binary loops were worse than the baseline.
- The implementation keeps the parser structure intact and only rewrites measured precedence layers; equality and comparison parsing stay custom because they carry warnings and type-annotation behavior.

## Context

`TGocciaParser` parses binary expressions through a shared helper:

```pascal
function TGocciaParser.ParseBinaryExpression(
  const ANextLevel: TParseFunction;
  const AOperators: array of TGocciaTokenType): TGocciaExpression;
```

That helper is compact, but every precedence level pays for an object-method callback and an open-array operator scan. Parser phase timing from `GocciaBenchmarkRunner` showed enough parse cost in expression-heavy sources to justify measuring more direct loops before restructuring the parser.

All numbers in this spike were captured on 2026-04-29 on macOS `darwin/aarch64`, FPC 3.2.2, production build:

```bash
./build.pas --prod benchmarkrunner
```

Synthetic inputs were piped through stdin with:

```bash
GOCCIA_BENCH_CALIBRATION_MS=1 GOCCIA_BENCH_ROUNDS=1 \
  ./build/GocciaBenchmarkRunner - --format=json --no-progress --jobs=1
```

The benchmark runner executes the source before reporting JSON, so synthetic cases were written to parse successfully and then execute a no-op benchmark.

## What Was Tried

### Direct `Peek.TokenType` Binary Loops

The successful spike rewrote the measured precedence layers from `ParseBinaryExpression(...)` calls to local loops:

- `LogicalOr`
- `NullishCoalescing`
- `LogicalAnd`
- `BitwiseOr`
- `BitwiseXor`
- `BitwiseAnd`
- `Shift`
- `Addition`
- `Multiplication`

Each loop checks `Peek.TokenType` directly and consumes the operator with `Advance` only after a match. This removes open-array matching from the common no-operator path and avoids the helper's method callback.

Equality and comparison parsing were not converted. They are not just plain binary loops: equality emits warnings for unsupported loose equality, and comparison also handles `is` match patterns plus type-annotation syntax.

### Direct Loops Using Repeated `Check(...)`

An earlier direct-loop variant used repeated `Check(...)` calls in conditions like:

```pascal
while Check(gttPlus) or Check(gttMinus) do
```

That version was rejected. It improved one class-heavy case slightly, but regressed expression-heavy medians because it still paid repeated `IsAtEnd` and `Peek` checks and expanded code size without removing enough overhead.

### `Advance inline`

Marking `TGocciaParser.Advance` as `inline` was rejected. Production builds already enable `{$optimization autoInline}` through `Shared.inc`, and explicit inlining regressed parse medians in the measured cases.

### Eager Source Text Removal

Short-circuiting `ExtractSourceRange` was used as an upper-bound spike for lazy function/method source text. It regressed production parse medians, so lazy source-text materialization is not justified as a parser performance change from this data.

## Synthetic Results

Fifteen production runs per case. Values are median parser phase duration.

| Case | Baseline | Direct `Peek.TokenType` | Delta |
|---|---:|---:|---:|
| vars | 33.49ms | 29.83ms | 10.9% faster |
| binary | 36.64ms | 35.01ms | 4.4% faster |
| calls | 37.62ms | 33.67ms | 10.5% faster |
| objects | 40.81ms | 34.69ms | 15.0% faster |
| arrays | 39.42ms | 38.00ms | 3.6% faster |
| destructuring | 32.03ms | 28.50ms | 11.0% faster |
| classes | 22.01ms | 18.52ms | 15.9% faster |
| arrows | 27.72ms | 25.71ms | 7.3% faster |
| exports | 13.94ms | 11.31ms | 18.9% faster |

Two additional synthetic shapes, `functions` and `control`, were excluded from the final comparison because their generated sources failed before JSON output was available. They need a parser-only timing harness or corrected executable sources before they can be used as stable parser map cases.

## Existing Benchmark Files

Ten production runs per benchmark file. Values are median parser phase duration.

| File | Baseline | Direct `Peek.TokenType` | Delta |
|---|---:|---:|---:|
| `benchmarks/arrays.js` | 0.42ms | 0.38ms | 9.5% faster |
| `benchmarks/classes.js` | 0.68ms | 0.61ms | 10.3% faster |
| `benchmarks/destructuring.js` | 0.44ms | 0.38ms | 13.6% faster |
| `benchmarks/jsx.jsx` | 0.34ms | 0.30ms | 11.8% faster |
| `benchmarks/modules.js` | 0.08ms | 0.07ms | 12.5% faster |
| `benchmarks/objects.js` | 0.13ms | 0.12ms | 7.7% faster |
| `benchmarks/regexp.js` | 0.14ms | 0.12ms | 14.3% faster |
| `benchmarks/strings.js` | 0.32ms | 0.30ms | 6.3% faster |

These files are small parser workloads, so absolute timings are sub-millisecond and noisier than the synthetic map. The direction still matched the larger parser-focused cases.

## What Was Not Tried

### Structural Parser Splits

The spike did not split `Goccia.Parser.pas` into multiple units. A structural split may improve maintainability later, but it is a larger ownership change and should follow confirmed hot spots. The measured win here is local and does not require a new public unit API.

### Parser-Only Timing Harness

The existing benchmark runner was sufficient for the main comparison, but it executes the source before emitting JSON. A future parser-only harness would make it easier to measure invalid programs, unsupported syntax, and control-flow-heavy sources without requiring runtime-safe generated code.

### Whole-Parser Table-Driven Precedence Parsing

A Pratt/table-driven expression parser was not attempted. It would be a larger semantic rewrite with higher regression risk. The direct-loop change captures the measured overhead while preserving the current recursive descent structure.

### Lexing Changes

Lexing was measured separately but not optimized in this spike. The target was parser phase time only.

## Decision

Implement direct `Peek.TokenType` loops for the measured binary-expression precedence layers and leave the rest of the parser structure unchanged. This gives a consistent parser speedup with a small behavioral surface and preserves the current recursive descent design.
