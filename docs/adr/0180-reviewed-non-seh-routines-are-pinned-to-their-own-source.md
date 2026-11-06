# 0180 - A reviewed non-SEH routine is pinned to its own source text, not its whole file

**Date:** 2026-11-06
**Area:** `build`, `windows`, `exceptions`
**Issue:** [#2111](https://github.com/frostney/GocciaScript/issues/2111)
**Refines:** [ADR 0169](0169-win32-safeseh-post-link.md)

## Context

ADR 0169's post-link step refuses any project text section whose routine name
looks like an exception handler unless the manifest classifies it. A
`source-nonseh` entry clears a reviewed ordinary routine. In manifest format 1
its SHA-256 covered the routine's whole source file, so any edit to that file
failed the i386-win32 build. Every one of the eight commits that touched the
manifest after SafeSEH landed only refreshed those hashes, and none of them
changed a routine's classification. `Goccia.VM.pas` holds eight of the
routines and is the engine file edited most often. Refreshing a whole-file hash
after an unrelated edit also teaches contributors to refresh it without
reviewing the routine, which weakens the review the pin exists for.

## Decision

Manifest format 2 (`FPC-SAFESEH 2`) binds each `source-nonseh` digest to the
implementation of the routine its section names, from the first token of its
header through the semicolon after its final `end`. CRLF and CR line endings
are normalised to LF; comments and whitespace inside the routine stay in the
digest. A format 1 manifest is refused, not read as routine digests.

The locator in `source/shared/FPCRoutineSource.pas` finds the routine from the
section name and refuses, rather than falling back to a broader or empty span,
whenever it cannot be sure the text it hashes is the text that was compiled:

- It accepts unit-level routines and methods of plain types in a unit or
  program, and the source must declare the section's unit or program. Nested
  routines and methods of nested or generic types are refused.
- It does not decode the section's parameter suffix (`$crc…` or the parameter
  types). The name must therefore have exactly one implementation in its
  scope: overloads are refused, and so is a nested routine with the same name
  as a reviewed unit-level routine.
- It lexes the whole file in Delphi mode and parses it through the final
  `end.`. Text it cannot parse, a routine keyword inside a statement block,
  another `{$mode}` and `{$modeswitch nestedcomments}` are refused, so no
  implementation can go unseen.
- An include inside the routine is refused. Every other include must resolve
  (beside its includer, then `source/units`, `source/shared` and `souffle`, the
  `-Fi` directories of `config.cfg` and CI) and must contain no routine or
  module keyword, so it cannot hold an implementation the parse misses. Macros
  are refused.
- The parse reads every branch of a conditional, so in the implementation
  part each conditional group must leave the parse unchanged whichever branch
  the compiler takes. A group must stay within one routine's statements at one
  block depth, one declaration level or type body, or one uses clause, and at
  declaration level each branch must begin and end next to `;` or a section
  keyword. A dead branch therefore cannot hide the compiled routine, and the
  digest covers every branch inside the reviewed routine.

`safeseh --digests [manifest]` prints every entry with its routine's current
digest, line range and status, and never rewrites the manifest. Unknown
handler-like names, owner binding, and the RTL object hashes are unchanged.

## Consequences

An edit outside the pinned routines needs no manifest change. An edited,
renamed or removed pinned routine still fails finalization and check, naming
the routine and its file. The contributor reviews the routine again before
copying its line from `safeseh --digests`.

The digest covers only the implementation. A declaration elsewhere, for
example a calling convention in the interface or class body, and code
elsewhere that could install the routine as a handler, are outside it.
Format 1 also covered the rest of the same file, though not other files. The
name rule remains a review checkpoint, not a proof that no frame registers the
routine; that proof is the machine-code analysis #2111 leaves out of scope.

A construct the locator cannot parse fails the Win32 build of a file that has
a pinned routine. Fix it by restructuring the code or by extending the locator
with tests, never by weakening a refusal.

[Win32 exception metadata](../contributing/win32-safeseh.md) owns the
procedure.
