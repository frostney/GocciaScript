# 0163 - goccia:ast reports the committed token stream, as typed ranges, on request

**Date:** 2026-10-27
**Area:** `runtime`

## Context

[ADR 0117](0117-javascript-visible-ast-module.md) gave `goccia:ast` a
statement tree and withheld everything below it: expressions, names and
literal values. That leaves a consumer no way to find where a string, a
template or a regular-expression literal is. `comments` covers comments, and
nothing covers the rest.

The consumer this was designed against is a check that finds every
`process.env.NAME` in a TypeScript tree. Its whole difficulty is that the
same text inside a string, a template's text, a regex or a comment does not
count, while inside a template's `${...}` it does. It is a 600-line
hand-written lexer, and its only job is to blank those ranges out. The
formatter measured in
[#1549](https://github.com/frostney/GocciaScript/issues/1549) hit the same
wall from the other side: it tokenized each file again in JavaScript, and
every bug found while building it was one of those re-derivations.

Two of the things such a lexer has to get right are exactly the ones ADR 0117
kept in the engine: whether a `/` divides or opens a regex, and where a
template's text ends and its substitution's code begins. Neither can be
decided by looking at characters. After `if (ok)` a `/` opens a regex; after
`(ok)` in an expression it divides. Only the parser knows which, and this
engine's lexer is driven by it: the parser asks for each token under the
lexical goal it expects (`glgInputElementDiv`, `glgInputElementRegExp`,
`glgInputElementTemplateTail`), the way ES2026 §12 describes.

The pipeline already had half of this. `TGocciaSourcePipelineOptions
.CollectTokens` copies the committed token stream for the provider package
crawl, with each token's type and lexeme but not its position.

## Decision

**`parse(text, { tokens: true })` also returns `tokens`: every token and
comment of the file, in source order, as `{ type, start, end }`.** Without the
option the result has no `tokens` property, the pipeline copies no token and
the JSX transformer records none.

### The source is the stream the parse used

The tokens are the lexer's committed stream, not a second scan. A speculative
parse rewinds the lexer whenever a token it scanned could be classified
differently under another goal (`HasGoalSensitiveTokenSince`), and every other
rewind truncates the token list to its checkpoint, so the list a successful
parse leaves behind is exactly the one it consumed, each token lexed under
the goal the parser chose. Copying it with offsets is all `tokens: true`
costs the lexer.

Two kinds of region are scanned without being parsed, and both were checked
against this claim. A construct the profile disables (`var`, a `function`
declaration, a `while` loop without its flag) is skipped token by token under
the regular-expression goal when the host asks for warnings, which would read
`a / b / c` as a regex. `parse` runs with that warning off, so the parser
raises a `SyntaxError` at the construct instead of skipping it, and no such
region reaches `tokens`. A TypeScript `type` or `interface` declaration is
skipped under the same goal in every profile, but type syntax has no division
and no regular expression, so no token in it is classified differently. A
template literal type in a `type` alias is skipped through the template-aware
path; one in an `interface` body is a parse error today, before and after this
change.

Comments come from the lexer's existing trivia sink, as `comments` does. The
hashbang is a comment too (ES2026 §12.5) and was not recorded anywhere; the
sink now records it under its own kind. It appears in `tokens` only, so
`comments` keeps the two kinds it has always reported.

### Types are the grammar's names, and coarse

| Type | Covers |
|------|--------|
| `Identifier` | An IdentifierName that is not a reserved word, including contextual words (`let`, `async`, `of`, `type`) and a reserved word spelled with an escape |
| `PrivateIdentifier` | `#name` |
| `Keyword` | An ES2026 §12.7.2 ReservedWord, by spelling: `x.default` has a `Keyword` token |
| `Punctuator` | Every operator and punctuator, as the lexer scanned it |
| `NumericLiteral` | Including BigInt literals |
| `StringLiteral` | Quotes included; also a JSX attribute string |
| `RegularExpressionLiteral` | Slashes and flags included |
| `NoSubstitutionTemplate`, `TemplateHead`, `TemplateMiddle`, `TemplateTail` | A template's text with its delimiters |
| `LineComment`, `BlockComment`, `HashbangComment` | Delimiters included |
| `JSXIdentifier`, `JSXText` | Tag and attribute names, and text between tags |

The names are the lexical grammar's (ES2026 §12.4-§12.9, and the JSX
specification for the two JSX kinds), so a reader can look each one up. The
finer distinction the lexer makes — which operator, which keyword — is in the
text the range selects, and keeping it out keeps the lexer's token enum, an
implementation detail, out of the API.

`Keyword` is decided by spelling against the specification's list, not by
the lexer's token type: the lexer promotes some contextual words (`let`,
`static`, `as`, `from`) to keyword tokens and leaves others (`async`, `of`)
as identifiers, for its own convenience. A role-based answer would need the
parse tree this module withholds.

Three tokens are reshaped from the lexer's to match the grammar:

- **A template continuation starts at its `}`.** The lexer reads the `}` that
  closes a substitution as an ordinary token and scans the continuation from
  the character after it. ES2026 §12.9.6 puts that `}` at the start of
  `TemplateMiddle` and `TemplateTail`, and following the grammar is what lets
  a template's parts and its substitutions' code never share a range: blank
  every template token and what is left is exactly the substitutions.
- **`#name` is one token**, as `PrivateIdentifier` is in ES2026 §12.7. The
  lexer scans `#` and the name separately.
- **`>>` stays one token** even where it closes two type-argument lists. The
  parser counts it as two closers rather than splitting it, so there is no
  second token to report.

### One coordinate system, including JSX

Offsets index `source`, as every other offset does
([ADR 0118](0118-original-file-source-ranges.md)). With `jsx: true` the lexer
never saw the file, so its tokens are measured in the transform, and part of
the transform is text the file does not contain.

- **A lexer token is reported only when it is a verbatim copy.**
  `TGocciaOriginMap.MapVerbatim` answers whether a whole range lies inside one
  run of the origin map. Rounding outward, which is right for a node's range,
  is wrong here: a token that is half synthesized has no text of its own in
  the file. Everything the transformer synthesized — `createElement`, `null`,
  the string a text child became — is dropped this way.
- **The JSX transformer reports the JSX.** Tag names, attribute names, the
  punctuation of tags and expression containers, attribute strings and text
  are consumed by the transformer and never reach the lexer. It is the only
  code that decides where they are, so it records them, in the file's
  offsets, through an opt-in sink in the same shape as the lexer's comment
  sink. The code inside a container is copied, so the lexer tokenizes it and
  none of it is in the sink.
- **The comments the transformer drops are tokens.** A comment between JSX
  attributes and one in a container that holds nothing else never reach the
  lexer, which is why ADR 0118 records them as missing from `comments`. The
  transformer sees them, so it records them in the sink. They are in
  `tokens`; `comments` is unchanged.
- `JSXText` is the raw run between tags, whitespace and all, as the JSX
  specification's `JSXText` is; the transformer trims it for the value, not
  for the range.

### What is withheld

- **Values.** A string's cooked value, a regex's pattern, a number's value:
  the consumer slices `source`, as ADR 0117 argues for names.
- **`loc`.** Each `loc` is three objects. A file has dozens of tokens per
  statement node, and `loc` would roughly double what `tokens: true` costs. A
  consumer that reports a finding at a token has its offset and the source.
- **A second encoding.** Parallel typed arrays (type, start, end) would be
  cheaper to build than an object per token. #1549 asked for objects, which
  is what every JavaScript tokenizer API hands out; an encoding for bulk
  consumers can be added beside it when one measures the need.

### Rejected

- **Re-scanning after the parse**, in Pascal or JavaScript: a scanner without
  the parser has no goal to lex `/` or `}` under. It would be the second
  scanner ADR 0117's Consequences ruled out.
- **Exposing the lexer's token types.** Their names and granularity follow
  what the parser needs (`gttLeftParen`, keyword tokens for some contextual
  words), and would freeze the parser's internals into the API.

## Consequences

- A tool that needs to know what is code can be a GocciaScript program without
  its own lexer. Blanking the ranges of every literal, template and comment
  token leaves exactly the code, with offsets preserved.
- **Verified, not asserted.** `examples/_experimental/ast-tokens.mjs` checks
  four claims per file: tokens are ordered, non-empty and non-overlapping;
  every character no token covers is whitespace; each token's text has its
  type's shape; and every comment in `comments` is a comment token with the
  same range. Over this repository's `tests/`, `scripts/`, `examples/` and
  `website/src` — 1,480,574 tokens in 2,160 files, `.jsx` and `.tsx` among them — every
  claim holds.
- **The cost is paid only on request**, and is mostly allocation: one object,
  two numbers and three property descriptors per token. Over the
  repository's TypeScript that parses in the default profile with the
  compatibility flags on (`scripts/`, 1.69 M characters and 227,938 tokens;
  `website/src`, 0.73 M characters and 111,740 tokens), `tokens: true` added
  58% to 73% to the time of the plain parse, about 4 to 6 µs per token
  (fastest of 15 interleaved runs per mode, on a shared 10-core machine under
  load, so the spread is wide). Without the option nothing is collected: the
  pipeline copies no token and the transformer's sink returns at its first
  check.
- The JSX transformer's token sink has to stay in step with its scanning: a
  JSX construct the transformer learns to consume needs a matching
  `NoteToken`, or its characters fall out of `tokens`. The verification
  example reports that as an uncovered character, which is how it would be
  found.

## Related

- [ADR 0117](0117-javascript-visible-ast-module.md) — what the module exposes
- [ADR 0118](0118-original-file-source-ranges.md) — the one coordinate system
- [#1923](https://github.com/frostney/GocciaScript/issues/1923) — this change;
  part of [#1549](https://github.com/frostney/GocciaScript/issues/1549)
