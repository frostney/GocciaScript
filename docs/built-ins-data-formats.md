# Data Format Built-ins

*JSON, JSON5, YAML, JSONL, CSV, TSV, and TOML API reference.*

## Executive Summary

- **JSON remains standard** — `JSON` is the ECMAScript global object and is always registered by the engine.
- **Non-standard formats are modules** — JSON5, YAML, JSONL, CSV, TSV, and TOML runtime APIs are named-export-only `goccia:` modules, not auto-installed globals.
- **Namespace imports preserve the old call shape** — Use `import * as CSV from "goccia:csv"` when you want `CSV.parse(...)`; there is no default export.
- **File-backed imports are unchanged** — `.json5`, `.jsonc`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, and `.tsv` files keep their structured-data import behavior.
- **Shared parsers** — A runtime module and the file-backed import of its format use the same `parse` function.

## Runtime module access

The non-standard data-format runtime APIs are available as Goccia runtime modules:

```javascript
import * as CSV from "goccia:csv";
import * as JSON5 from "goccia:json5";
import * as JSONL from "goccia:jsonl";
import * as TOML from "goccia:toml";
import * as TSV from "goccia:tsv";
import * as YAML from "goccia:yaml";
```

These modules expose named exports only. Namespace imports give the familiar namespace-object shape (`CSV.parse(...)`, `TOML.parse(...)`), while named imports keep call sites explicit:

```javascript
import { parse as parseYAML, parseDocuments } from "goccia:yaml";
```

Default imports are intentionally unsupported for these modules.

## File-backed data inputs

Structured data imports (`.json`, `.json5`, `.jsonc`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, `.tsv`), text asset imports (`.txt`, `.md`), import maps, source maps, and CLI configuration files are loaded as raw bytes and tagged as UTF-8 before parsing. Parser-facing text stays on the shared UTF-8 text path so non-ASCII keys and values are preserved consistently across supported formats. The `goccia:` runtime-module migration does not change file-backed data imports.

## JSON

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

## JSON5 (`goccia:json5`)

| Method | Description |
|--------|-------------|
| `JSON5.parse(text, reviver?)` | Parse JSON5 text to a value, with optional reviver function that receives source text access |
| `JSON5.stringify(value, replacerOrOptions?, space?)` | Serialize a value with JSON5 syntax, including special numeric values and optional `quote` override |

`goccia:json5` is written in JavaScript (`source/engine/prelude/json5.js`), with a parser of its own: `JSON.parse(...)` (`Goccia.JSON`) stays strict while `JSON5.parse(...)` accepts comments, trailing commas, single-quoted strings, unquoted identifier keys, hexadecimal numbers, signed numbers, `Infinity`, `NaN`, line continuations, and ECMAScript whitespace extensions. Source text access works identically to `JSON.parse`: the reviver receives `(key, value, context)` where `context.source` preserves the raw JSON5 text for primitives (including extended literals like `Infinity`, `NaN`, `0xFF`, and `+1`).

The `stringify` export follows the reference `json5` library's formatting rules: unquoted identifier keys, single- or double-quoted strings (with optional `{ quote: "'" | '"' }` override), preserved `Infinity` / `-Infinity` / `NaN`, trailing commas when pretty-printing, `toJSON5()` preference over `toJSON()`, and the same replacer / space semantics as JSON plus the upstream JSON5 options-object form `{ replacer, space, quote }`.

Compatibility goal: GocciaScript is targeting full JSON5 parser compatibility
plus upstream-aligned stringify behavior. `GocciaTestRunner` runs the committed
generated upstream parser suite together with the local upstream-aligned
stringify suite.
See [Testing](testing.md#running-tests) for checkout preparation and
commands.

## YAML (`goccia:yaml`)

| Method | Description |
|--------|-------------|
| `YAML.parse(text)` | Parse YAML text; returns an array when the stream uses explicit `---` document markers |
| `YAML.parseDocuments(text)` | Parse a YAML stream and always return an array of documents |

After `import * as YAML from "goccia:yaml"`, `YAML.parse` hands the text to the engine-independent `TGocciaYAMLParser` in `Goccia.YAML`; the host returns the parsed `Goccia.DataTree` as JSON text, which `source/engine/prelude/datatree.js` rebuilds into values, so a node that YAML aliases share is one value, cycles included. When the stream has an explicit `---` document marker, `YAML.parse(...)` returns an array of the parsed documents; otherwise it returns the first document's value directly, as Bun does for one document. `YAML.parseDocuments(...)` is the always-array variant.

`TGocciaYAMLParser` composes the events of a YAML 1.2.2 scanner and event parser ported from saphyr-parser ([ADR 0195](adr/0195-yaml-is-parsed-by-a-port-of-saphyrs-scanner-and-event-parser.md)), so it reads the whole language: block and flow collections, explicit and implicit keys, block scalars with chomping and indentation indicators, line folding, every escape of double-quoted scalars, anchors and aliases, including self-referential alias graphs, comments, tabs where YAML allows them, multi-document streams and the `%YAML` and `%TAG` directives. Text that is not YAML 1.2.2 throws a `SyntaxError` naming the line and column. A directive applies to the document its `---` starts, so directives need that marker, and directives after document content need a `...` first. A `%YAML` directive must name version 1.x. Two rules are relaxed, as other YAML parsers relax them: a flow collection's closing `]` or `}` may sit at the indentation of the block node that holds it, and a mapping key that repeats takes the value it is last given, as `JSON.parse` does.

Plain scalars resolve by the YAML 1.2 core schema, with the extensions Bun's YAML also reads: digit separators (`1_000`), `0b` binary integers, and `<<` merge keys. The standard tags `!!str`, `!!int`, `!!float`, `!!bool`, `!!null`, `!!seq` and `!!map` convert or check their node, and the non-specific tag `!` makes a scalar a string.

Tagged values preserve runtime metadata through `.tagName` and `.value`. Custom tags wrap the parsed underlying value, `!!timestamp` validates ISO date/date-time scalars, and `!!binary` validates its base64 text and keeps that text as the wrapped value.

Because GocciaScript mappings are backed by string-keyed objects, a mapping's keys become strings: a scalar key its JavaScript string (`1` becomes `"1"`, an empty key `""`), and a collection key, such as an explicit `? [a, b]`, a stable JSON-like string.

Compatibility: the native `GocciaYAMLComplianceRunner` checks the parser against every case of the official `yaml-test-suite` at the pinned commit: the event stream of each valid case against its `test.event`, the parsed documents against its JSON values, and the rejection of each invalid case ([Testing](testing.md)). Pull-request, main-branch and release CI run it, and any failing case fails the run. Historical decision context lives in [Architecture Decision Records](adr/).

## JSONL (`goccia:jsonl`)

| Method | Description |
|--------|-------------|
| `JSONL.parse(textOrBytes)` | Parse newline-delimited JSON and return an array of all records |
| `JSONL.parseChunk(textOrBytes[, start[, end]])` | Parse as many complete JSONL records as possible and return `{ values, read, done, error }` |

After `import * as JSONL from "goccia:jsonl"`, `JSONL.parse(...)` and `JSONL.parseChunk(...)` are designed to match Bun's JSONL runtime API closely: blank lines are ignored, each non-empty line must be strict JSON, and `Uint8Array` input is supported alongside strings. `parseChunk(...)` returns the next resume offset in `read`, a `done` flag when the provided range was fully consumed, and a `SyntaxError` object in `error` when a delimited record is invalid. Incomplete trailing records are left unread so callers can append more data and resume parsing.

## CSV (`goccia:csv`)

| Method | Description |
|--------|-------------|
| `CSV.parse(text, options?, reviver?)` | Parse RFC 4180 CSV text into an array of objects (headers mode) or array of arrays |
| `CSV.parseChunk(text, options?, start?, end?)` | Parse CSV rows in the range and return `{ values, read, done, error }`; see below for the final row |
| `CSV.stringify(data, options?, replacer?)` | Convert an array of objects or arrays to CSV text |

**Options:** `{ delimiter: ',', headers: true, skipEmptyLines: false }`. The `delimiter` option supports any single character (e.g., `;` for European CSVs, `|` for pipe-delimited). All parsed values are strings — no type coercion.

**Reviver:** The optional reviver callback `(key, value, context)` is called for each cell. The `context` object contains `{ quoted: boolean, row: number, column: number }`. The `quoted` flag lets callers distinguish `""` (a quoted empty field) from `,` (an unquoted empty field), enabling patterns like converting unquoted empties to `null` while preserving quoted empties as `""`.

**Replacer:** The optional replacer callback `(key, value)` is called for each cell during `CSV.stringify`, enabling value transformation before serialization.

**Edge cases:** Empty fields are `""`, trailing delimiters create an extra `""` field, empty rows are preserved by default (opt-in `skipEmptyLines`), in headers mode short rows are padded with `""` and extra fields dropped (in array mode each row keeps its own length), and quoting follows RFC 4180 (fields containing the delimiter, `"`, or newline are enclosed in double quotes with `""` escaping).

**`parseChunk` and the final row:** CSV and TSV differ on a last row with no line terminator ([#1277](https://github.com/frostney/GocciaScript/issues/1277)). `CSV.parseChunk("a,b\n1,2\n3")` parses `3` as a row and returns `read: 9, done: true`; an unterminated quoted field is left unread and reported in `error`. `TSV.parseChunk("a\tb\n1\t2\n3")` leaves `3` unread (`read: 8, done: false`), the resume model JSONL uses.

## TSV (`goccia:tsv`)

| Method | Description |
|--------|-------------|
| `TSV.parse(text, options?, reviver?)` | Parse IANA TSV text into an array of objects (headers mode) or array of arrays |
| `TSV.parseChunk(text, options?, start?, end?)` | Parse as many complete TSV rows as possible and return `{ values, read, done, error }`; an unterminated final row is left unread |
| `TSV.stringify(data, options?, replacer?)` | Convert an array of objects or arrays to TSV text |

**Options:** `{ headers: true, skipEmptyLines: false }`. No `delimiter` option — TSV always uses tab.

**Reviver:** The optional reviver callback `(key, value, context)` is called for each cell. The `context` object contains `{ row: number, column: number }`. Unlike CSV, no `quoted` flag is provided since TSV uses backslash escaping rather than quoting.

**Replacer:** The optional replacer callback `(key, value)` is called for each cell during `TSV.stringify`, enabling value transformation before serialization.

TSV uses IANA `text/tab-separated-values` semantics, which differ fundamentally from CSV: instead of RFC 4180 double-quote escaping, TSV uses **backslash escaping** (`\t` for tab, `\n` for newline, `\r` for carriage return, `\\` for literal backslash). Unrecognized escape sequences preserve the backslash. The reviver, replacer, and edge case handling match CSV; `parseChunk` does not (see below).

CSV and TSV are one JavaScript prelude, `source/engine/prelude/delimited.js`, which shares option reading, callback handling and chunk results between the two formats and keeps each format's own escaping, reviver context and stringifier.

## TOML (`goccia:toml`)

| Method | Description |
|--------|-------------|
| `TOML.parse(text)` | Parse TOML 1.1.0 text into Goccia values |

After `import * as TOML from "goccia:toml"`, `TOML.parse` hands the text to the engine-independent `TGocciaTOMLParser` in `Goccia.TOML`, and `source/engine/prelude/datatree.js` rebuilds the parsed tree into values, as for YAML. The current TOML API supports strings (basic, literal, and multiline variants), integers, floats, booleans, arrays, inline tables, regular tables, arrays of tables, dotted keys, and TOML 1.1.0 date/time values. TOML multiline strings normalize recognized source newlines to LF (`\n`) regardless of the host platform, so the parsed value is stable across Linux, macOS, and Windows. File-backed inputs are read as raw bytes, decoded as UTF-8, and handed to the parser as UTF-16 text, so non-ASCII keys and values do not depend on compiler or operating-system code pages.

TOML date/time values currently map to validated string scalars rather than Temporal values. This keeps the runtime and module-import behavior stable for v1 while leaving room for future Temporal-aware interop.

TOML integers become JavaScript numbers, which are IEEE-754 doubles, so `TOML.parse` loses precision for an integer outside ±2^53: `9223372036854775807` becomes the double 2^63, which prints as `9223372036854776000`.

Compatibility: the parser passes every case of the official `toml-test`
TOML 1.1.0 suite at the pinned commit. The native `GocciaTOMLComplianceRunner`
checks the parser's tagged document tree (`ParseDocument`), which keeps each
scalar's TOML kind and canonical value, rather than the values `TOML.parse`
returns, so the integer limit above does not show there. Main-branch and
release CI run it on every target, and any failing case fails that run
([Testing — CI Integration](testing.md#ci-integration)). The runner verifies
and runs a prepared pinned `toml-test` checkout; see
[Testing](testing.md#running-tests). Historical decision context lives in
[Architecture Decision Records](adr/).
