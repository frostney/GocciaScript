# Data Format Built-ins

*JSON, JSON5, YAML, JSONL, CSV, TSV, and TOML API reference.*

### JSON (`Goccia.Builtins.JSON.pas`)

Implements the [ECMAScript JSON object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON) including source text access and Raw JSON.

| Method | Description |
|--------|-------------|
| `JSON.parse(text, reviver?)` | Parse JSON string to value, with optional reviver function that receives source text access |
| `JSON.stringify(value, replacer?, space?)` | Convert value to JSON string, with optional replacer (function or array) and indentation |
| `JSON.rawJSON(text)` | Create a raw JSON value object for verbatim serialization |
| `JSON.isRawJSON(value)` | Return `true` if the value was created by `JSON.rawJSON()` |

**Raw JSON:** `JSON.rawJSON(text)` creates a frozen, null-prototype object with a `rawJSON` property containing the original text. The input must be valid JSON representing a primitive value (string, number, boolean, or null) — objects and arrays are rejected. Leading/trailing whitespace and empty strings throw `SyntaxError`. During `JSON.stringify`, raw JSON values are emitted verbatim without re-serialization, enabling lossless round-tripping of values like large integers (`9007199254740993`) that exceed IEEE-754 `Number` precision. Replacer functions can return `JSON.rawJSON()` values. `JSON.isRawJSON(value)` checks for the internal `[[IsRawJSON]]` slot — objects that merely mimic the structure (e.g., `{ rawJSON: "123" }`) return `false`.

**Source text access:** When a reviver is provided, it receives three arguments: `(key, value, context)`. The `context` parameter is an object. For primitive JSON values (numbers, strings, booleans, `null`), the context contains a `source` property with the raw JSON text that produced the value — including quotes for strings, exact numeric notation, and escape sequences as written. For objects and arrays, the context object has no `source` property. This enables lossless round-tripping of numeric precision and format-aware value reconstruction.

The JSON parser is a recursive descent implementation. Special handling:

- `NaN` → `null` in stringify
- `undefined` → omitted in objects, `null` in arrays
- Functions → omitted in objects, `null` in arrays
- Symbols → omitted in objects, `null` in arrays
- Finite floating-point numbers are serialized with round-trip-safe decimal text when needed
- `toJSON()` is called before serializing object values
- Circular references throw `TypeError`

### JSON5 (`Goccia.Builtins.JSON5.pas`)

| Method | Description |
|--------|-------------|
| `JSON5.parse(text, reviver?)` | Parse JSON5 text to a value, with optional reviver function that receives source text access |
| `JSON5.stringify(value, replacerOrOptions?, space?)` | Serialize a value with JSON5 syntax, including special numeric values and optional `quote` override |

`JSON5.parse` delegates to the standalone `TGocciaJSON5Parser` utility in `Goccia.JSON5`. The JSON5 parser shares the same core capability-driven parser engine as strict JSON, so `JSON.parse(...)` stays strict while `JSON5.parse(...)` opts into comments, trailing commas, single-quoted strings, unquoted identifier keys, hexadecimal numbers, signed numbers, `Infinity`, `NaN`, line continuations, and ECMAScript whitespace extensions. Source text access works identically to `JSON.parse`: the reviver receives `(key, value, context)` where `context.source` preserves the raw JSON5 text for primitives (including extended literals like `Infinity`, `NaN`, `0xFF`, and `+1`).

`JSON5.stringify` delegates to `TGocciaJSON5Stringifier`, which reuses the same shared serialization core as strict JSON but switches to JSON5 formatting rules. That means unquoted identifier keys, single- or double-quoted strings (with optional `{ quote: "'" | '"' }` override), preserved `Infinity` / `-Infinity` / `NaN`, trailing commas when pretty-printing, `toJSON5()` preference over `toJSON()`, and the same replacer / space semantics as JSON plus the upstream JSON5 options-object form `{ replacer, space, quote }`.

Compatibility goal: GocciaScript is targeting full JSON5 parser compatibility plus upstream-aligned stringify behavior. `python3 scripts/run_json5_test_suite.py` now runs both the official `json5/json5` parser corpus and the local upstream-aligned stringify suite in one command, and `python3 scripts/run_json5_test_suite.py --harness=./build/GocciaJSON5Check` reuses a prebuilt parser decoder when you already have it.

### YAML (`Goccia.Builtins.YAML.pas`)

| Method | Description |
|--------|-------------|
| `YAML.parse(text)` | Parse YAML text; returns an array when the stream uses explicit `---` document markers |
| `YAML.parseDocuments(text)` | Parse a YAML stream and always return an array of documents |

`YAML.parse` delegates to the standalone `TGocciaYAMLParser` utility in `Goccia.YAML`, mirroring the JSON built-in's split between parser utility and global runtime surface. Its behavior now matches Bun's YAML runtime semantics: when the input uses explicit `---` document markers, `YAML.parse(...)` returns an array of parsed documents; otherwise it returns the first parsed document value directly. `YAML.parseDocuments(...)` is the always-array variant.

The current YAML parser also supports anchors, aliases, merge keys, self-referential alias graphs for mappings and sequences, multiline flow-style collections and flow edge cases such as single-pair mapping entries, empty implicit keys, and trailing commas, block scalars (`|`, `>`, chomping modifiers, and indentation indicators), multiline plain and quoted scalar folding, YAML 1.2 numeric scalar resolution (including base-prefixed integers, exponent forms, and validated digit separators), YAML double-quoted escapes (`\x`, `\u`, `\U`, line continuations, and YAML-specific escapes), `%YAML` / `%TAG` directives, and the standard tags `!!str`, `!!int`, `!!float`, `!!bool`, `!!null`, `!!seq`, `!!map`, `!!timestamp`, and `!!binary`. Directives are treated as document-preamble syntax and are rejected if they appear after document content without a document boundary.

Tagged values preserve runtime metadata through `.tagName` and `.value`. Custom tags wrap the parsed underlying value, `!!timestamp` validates ISO date/date-time scalars, and `!!binary` validates and decodes base64 text into the wrapped scalar value.

Explicit keys (`? key`) are supported, including omitted explicit values and zero-indented sequence values. Because GocciaScript mappings are backed by string-keyed objects, non-scalar YAML keys are canonicalized into stable JSON-like strings during parsing, and anchored mapping keys now parse instead of being rejected outright.

Compatibility goal: GocciaScript is targeting full YAML 1.2 support over time while keeping Bun-compatible YAML runtime behavior where practical. The current parser is still a partial implementation. The detailed conformance snapshot lives in [docs/decision-log.md](decision-log.md), and the official parse-validity check can be rerun with `python3 scripts/run_yaml_test_suite.py`.

### JSONL (`Goccia.Builtins.JSONL.pas`)

| Method | Description |
|--------|-------------|
| `JSONL.parse(textOrBytes)` | Parse newline-delimited JSON and return an array of all records |
| `JSONL.parseChunk(textOrBytes[, start[, end]])` | Parse as many complete JSONL records as possible and return `{ values, read, done, error }` |

`JSONL.parse(...)` and `JSONL.parseChunk(...)` are designed to match Bun's JSONL runtime surface closely: blank lines are ignored, each non-empty line must be strict JSON, and `Uint8Array` input is supported alongside strings. `parseChunk(...)` returns the next resume offset in `read`, a `done` flag when the provided range was fully consumed, and a `SyntaxError` object in `error` when a delimited record is invalid. Incomplete trailing records are left unread so callers can append more data and resume parsing.

### CSV (`Goccia.Builtins.CSV.pas`)

| Method | Description |
|--------|-------------|
| `CSV.parse(text, options?, reviver?)` | Parse RFC 4180 CSV text into an array of objects (headers mode) or array of arrays |
| `CSV.parseChunk(text, options?, start?, end?)` | Parse as many complete CSV rows as possible and return `{ values, read, done, error }` |
| `CSV.stringify(data, options?, replacer?)` | Convert an array of objects or arrays to CSV text |

**Options:** `{ delimiter: ','  headers: true, skipEmptyLines: false }`. The `delimiter` option supports any single character (e.g., `;` for European CSVs, `|` for pipe-delimited). All parsed values are strings — no type coercion.

**Reviver:** The optional reviver callback `(key, value, context)` is called for each cell. The `context` object contains `{ quoted: boolean, row: number, column: number }`. The `quoted` flag lets callers distinguish `""` (a quoted empty field) from `,` (an unquoted empty field), enabling patterns like converting unquoted empties to `null` while preserving quoted empties as `""`.

**Replacer:** The optional replacer callback `(key, value)` is called for each cell during `CSV.stringify`, enabling value transformation before serialization.

**Edge cases:** Empty fields are `""`, trailing delimiters create an extra `""` field, empty rows are preserved by default (opt-in `skipEmptyLines`), ragged rows are padded with `""`, and quoting follows RFC 4180 (fields containing the delimiter, `"`, or newline are enclosed in double quotes with `""` escaping).

### TSV (`Goccia.Builtins.TSV.pas`)

| Method | Description |
|--------|-------------|
| `TSV.parse(text, options?, reviver?)` | Parse IANA TSV text into an array of objects (headers mode) or array of arrays |
| `TSV.parseChunk(text, options?, start?, end?)` | Parse as many complete TSV rows as possible and return `{ values, read, done, error }` |
| `TSV.stringify(data, options?, replacer?)` | Convert an array of objects or arrays to TSV text |

**Options:** `{ headers: true, skipEmptyLines: false }`. No `delimiter` option — TSV always uses tab.

**Reviver:** The optional reviver callback `(key, value, context)` is called for each cell. The `context` object contains `{ row: number, column: number }`. Unlike CSV, no `quoted` flag is provided since TSV uses backslash escaping rather than quoting.

**Replacer:** The optional replacer callback `(key, value)` is called for each cell during `TSV.stringify`, enabling value transformation before serialization.

TSV uses IANA `text/tab-separated-values` semantics, which differ fundamentally from CSV: instead of RFC 4180 double-quote escaping, TSV uses **backslash escaping** (`\t` for tab, `\n` for newline, `\r` for carriage return, `\\` for literal backslash). Unrecognized escape sequences preserve the backslash. The reviver, replacer, `parseChunk`, and edge case handling match CSV.

### TOML (`Goccia.Builtins.TOML.pas`)

| Method | Description |
|--------|-------------|
| `TOML.parse(text)` | Parse TOML 1.1.0 text into Goccia values |

`TOML.parse` delegates to the standalone `TGocciaTOMLParser` utility in `Goccia.TOML`, mirroring the JSON and YAML split between parser utility and runtime surface. The current TOML surface supports strings (basic, literal, and multiline variants), integers, floats, booleans, arrays, inline tables, regular tables, arrays of tables, dotted keys, and TOML 1.1.0 date/time values. TOML multiline strings normalize recognized source newlines to LF (`\n`) regardless of the host platform, so the parsed value is stable across Linux, macOS, and Windows. File-backed TOML inputs are treated as UTF-8 text on every platform, and the raw file text stays `UTF8String` until the parser consumes it. That type choice is intentional: in this FreePascal configuration plain `string` is still `AnsiString`, while `UTF8String` is the explicit UTF-8-tagged ansistring we use to preserve non-ASCII keys and values across Windows and non-Windows targets.

TOML date/time values currently map to validated string scalars rather than Temporal values. This keeps the runtime and module-import behavior stable for v1 while leaving room for future Temporal-aware interop.

Compatibility goal: GocciaScript is targeting full TOML 1.1.0 support over time. The detailed conformance notes live in [docs/decision-log.md](decision-log.md), and the official `toml-test` rerun command is `python3 scripts/run_toml_test_suite.py` or `python3 scripts/run_toml_test_suite.py --harness=./build/GocciaTOMLCheck` when you already have the decoder harness built.
