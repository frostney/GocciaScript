# Tooling

*Auto-formatting, editor configuration, and platform-specific pitfalls for FreePascal development.*

## Executive Summary

- **Auto-formatter** — `./format.pas` auto-fixes uses clauses, PascalCase naming, parameter prefixes, and stray spaces; runs via Lefthook pre-commit hook
- **Editor config** — `.editorconfig` + VSCode/Cursor extensions for zero-config formatting on save
- **Platform pitfalls** — FPC 3.2.2 `Int64`→`Double` conversion bugs (all platforms + AArch64-specific); endian-dependent byte indexing

## Auto-Formatting

The project includes `./format.pas`, an instantfpc script that formats Pascal source files. It runs automatically as a pre-commit hook via [Lefthook](https://github.com/evilmartians/lefthook) and can also be invoked manually — no build step needed.

### Setup

Install Lefthook as described in [Workflow — Local setup](workflow.md#local-setup), then register hooks with `lefthook install`.

### Manual Usage

```bash
# Format all project Pascal files
./format.pas

# Format specific files
./format.pas units/Goccia.Engine.pas

# Check only (exit 1 if changes needed)
./format.pas --check
```

### What It Enforces

All of the following are **auto-fixed** (not just warned about):

- **Uses clauses**: one unit per line, grouped (System > Third-party > Project > Relative), alphabetically sorted within each group, blank line between groups. Units with an `in` path are always in the Relative group; `Goccia.*` units are Project; known FPC standard library units are System; everything else is Third-party.
- **Function naming**: capitalizes the first letter of function, procedure, constructor, and destructor names to enforce PascalCase. Renames all references within the same file. External C bindings are excluded.
- **Parameter naming**: adds the `A` prefix to multi-letter parameters (e.g., `Value` -> `AValue`) and renames all references within the function scope (declaration, local variables, and body). Single-letter parameters and Pascal keyword conflicts are skipped.
- **Stray spaces**: removes spurious spaces before `;`, `)`, and `,` (e.g., `string ;` -> `string;`). String literals and comments are left untouched.

## Editor Configuration

### `.editorconfig`

The project uses `.editorconfig` for consistent formatting:

- **Indent:** 2 spaces (no tabs)
- **Line endings:** LF
- **Trailing whitespace:** trimmed
- **Final newline:** inserted
- **Charset:** UTF-8

### VSCode / Cursor Setup

The repository includes `.vscode/settings.json` and `.vscode/extensions.json` for a zero-config experience in VSCode and Cursor.

#### Recommended Extensions

Open the project and accept the "Install Recommended Extensions" prompt, or install them manually:

| Extension | ID | Purpose |
|-----------|----|---------|
| Pascal | `alefragnani.pascal` | Syntax highlighting, code navigation, and symbol search for Pascal/Delphi |
| Run on Save | `emeraldwalk.RunOnSave` | Triggers `./format.pas` automatically when a `.pas` or `.dpr` file is saved |
| EditorConfig | `editorconfig.editorconfig` | Applies `.editorconfig` rules (indent size, line endings, etc.) |

These are declared in `.vscode/extensions.json` so VSCode/Cursor will prompt to install them on first open.

#### Format on Save

`.vscode/settings.json` configures the [Run on Save](https://marketplace.visualstudio.com/items?itemName=emeraldwalk.RunOnSave) extension to run `./format.pas` on every `.pas` and `.dpr` file when saved. This keeps code style consistent without manual intervention — the formatter fixes uses clause ordering, PascalCase naming, parameter prefixes, and stray spaces in the background.

The `runOnSave` command runs silently (`"runIn": "backend"`), so it will not open a terminal or interrupt your workflow. The file is re-read by the editor after formatting, so changes appear immediately.

> **Note:** This requires `instantfpc` (ships with FreePascal) to be on your `PATH`. If you installed FreePascal via the methods in [Getting Started](../../README.md#getting-started), this is already the case.

#### How the Layers Work Together

| Layer | When it runs | What it does |
|-------|-------------|--------------|
| `.editorconfig` | While typing | Sets indent size, line endings, trailing whitespace, charset |
| `runOnSave` | On file save | Runs `./format.pas` to auto-fix code conventions |
| Lefthook pre-commit | On `git commit` | Runs `./format.pas` on staged files as a safety net |
| CI `--check` | On push / PR | Fails the build if any file needs formatting |

All four layers enforce the same rules, providing defence in depth. The typical developer experience is: EditorConfig handles whitespace while you type, format-on-save fixes everything else when you save, and the pre-commit hook and CI catch anything that slips through.

## Platform-Specific Pitfalls

### `Int64` to `Double` Conversion on FPC 3.2.2

FPC 3.2.2 has **two bugs** affecting `Int64` -> `Double` conversion. Bug A is a Delphi-mode front-end issue that affects **all platforms**. Bug B is an AArch64-specific codegen issue.

#### Bug A: `Double(Int64Var)` bit reinterpretation — [FPC #35886](https://gitlab.com/freepascal.org/fpc/source/-/issues/35886)

In `{$mode delphi}`, an explicit `Double(Int64Var)` cast performs a **Turbo Pascal-style bit reinterpretation** instead of a value conversion. This produces garbage floating-point values (e.g., `Double(Int64(1000))` yields `~4.94e-315` instead of `1000.0`). This is a compiler front-end bug in `defcmp.pas` that affects all platforms in Delphi mode. Fixed in FPC trunk (3.3.1, [commit `1da43f67`](https://gitlab.com/freepascal.org/fpc/source/-/commit/1da43f67d40dd92ea2fb1cfa327d6088fa838aa7)) but **not backported** to 3.2.x.

#### Bug B: `Int64 * 1.0` wrong results near +/-2^31 (AArch64 only)

Mixed `Int64 * Double` arithmetic produces **wrong results** for `Int64` values near the `LongInt` boundary (+/-2,147,483,648). This affects all arithmetic operators (`*`, `+`, `-`, `/`) where one operand is `Int64` and the other is `Double`. FPC appears to use a 32-bit `SCVTF` instruction instead of 64-bit when promoting `Int64` through arithmetic expressions. This is AArch64-specific and has **not yet been reported upstream**.

```text
// Observed on FPC 3.2.2 AArch64, all optimization levels:
Int64(-2147483647) * 1.0  ->  -2147483648   // WRONG (should be -2147483647)
Int64(-2147483649) * 1.0  ->  -2147483648   // WRONG (should be -2147483649)
Int64( 2147483649) * 1.0  ->   2147483648   // WRONG (should be  2147483649)
Int64(-3000000000) * 1.0  ->  -3000000000   // correct (far from boundary)
```

#### Safe conversion

Use **implicit assignment** or **function parameter passing** — both use the correct 64-bit conversion path and are unaffected by either bug:

```pascal
// WRONG — bit reinterpretation in Delphi mode (Bug A)
Result := Double(FEpochMilliseconds) * 1000000.0;

// WRONG — wrong results near +/-2^31 on AArch64 (Bug B)
Result := FEpochMilliseconds * 1.0;

// CORRECT — implicit assignment
var D: Double;
D := FEpochMilliseconds;
Result := D * 1000000.0;

// CORRECT — implicit promotion at function call boundary
// When passing Int64 to a function/constructor that takes Double,
// FPC generates the correct 64-bit SCVTF instruction.
Result := TGocciaNumberLiteralValue.Create(SomeInt64Value);
```

This affects any code that converts `Int64` fields to `Double` for floating-point arithmetic. Note that `Int64 / Int64` is safe — FPC's `/` operator already returns `Extended` for integer operands, so no explicit promotion is needed for division.

### Endian-Dependent Byte Indexing

Do **not** inspect raw byte arrays of `Double` values to check the sign bit (e.g., `Bytes[7] and $80`). This assumes little-endian byte layout and breaks on big-endian platforms.

Instead, overlay the `Double` with `Int64 absolute` and test via integer sign:

```pascal
// WRONG — assumes little-endian byte order
var V: Double; Bytes: array[0..7] of Byte absolute V;
begin
  Result := (V = 0.0) and ((Bytes[7] and $80) <> 0);
end;

// CORRECT — endian-neutral sign bit check
var V: Double; Bits: Int64 absolute V;
begin
  Result := (V = 0.0) and (Bits < 0);
end;
```

This works because `Int64` and `Double` share the same sign bit position (bit 63) at the integer level, regardless of byte ordering.
