# Canonical UTF-16 text with explicit encoded-byte boundaries

**Date:** 2026-07-19
**Area:** `engine`

ECMAScript Strings and source text are sequences of UTF-16 code units, including lone surrogates, while FPC and Delphi expose different historical string and code-page behaviours. GocciaScript therefore uses Unicode Pascal `string` as its only in-memory representation of ECMAScript text and uses `TBytes` for encoded or binary data. Source files, configuration, and structured-text inputs decode UTF-8 strictly before entering the engine; replacement decoding is used only by APIs whose specification requires it. Public and internal text APIs do not expose `UTF8String`, byte-backed strings, or parallel ANSI/Unicode modes.

Encoding is owned by explicit boundary adapters rather than compiler code-page conversion. Host paths remain Unicode text and platform adapters use the host's native path APIs; Windows command lines and environment data use wide APIs, while Unix-family byte boundaries use strict UTF-8. FFI `utf8string` values validate UTF-8, lone surrogates, embedded NUL, and ASCII-only symbol names explicitly. Goccia bytecode stores string constants as versioned UTF-16LE code units with no compatibility reader for the earlier byte-backed representation.

Source spans and String indices count UTF-16 code units. Lexical operations may consume a valid surrogate pair as one source character where ECMA-262 requires code-point interpretation, but offsets remain code-unit based. Template tokens carry separate raw text, cooked text, and cooked-valid state; sentinel code units are not reserved. Unicode casing, normalization, and well-formedness are engine semantics, with ICU permitted as an optional acceleration but not as the sole semantic implementation. The normative baseline is ECMA-262 ES2026 snapshot `0248456c758431e4bb8e5d26333ff1865123c9cd`, especially §6.1.4 and §22.1.3.15.
