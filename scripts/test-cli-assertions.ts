#!/usr/bin/env bun
/**
 * test-cli-assertions.ts
 *
 * Unit tests for scripts/test-cli/assertions.ts — the shared output-matching
 * helpers used by every scripts/test-cli*.ts harness.
 *
 * `containsLine` is the reason this file exists. It used to be implemented as
 * `s.includes("\n" + value + "\n")`, which cannot match the first line of a
 * capture (no preceding newline) nor the last line of a capture that lacks a
 * trailing newline. Harness authors compensated with local wrappers that
 * prepended "\n" to the capture, which hid the defect instead of fixing it.
 * These cases pin both halves of the contract: every line matches, including
 * the boundaries, and the match stays whole-line ("ok" never matches "not ok").
 *
 * Run: bun run scripts/test-cli-assertions.ts
 */

import { containsLine, normalizeLineEndings } from "./test-cli/assertions";

let passed = 0;

function assert(condition: unknown, message: string): void {
  if (!condition) throw new Error(`Assertion failed: ${message}`);
  passed++;
}

function assertEqual<T>(actual: T, expected: T, message: string): void {
  if (actual !== expected)
    throw new Error(
      `Assertion failed: ${message}\n  expected: ${JSON.stringify(expected)}\n  actual:   ${JSON.stringify(actual)}`,
    );
  passed++;
}

// -- Line position: first, middle, last ------------------------------------------

console.log("containsLine() matches every line position...");
{
  const capture = "first\nmiddle\nlast\n";

  // The regression the helper was fixed for: the first line has no preceding
  // newline, so the old `\n<value>\n` search could never match it.
  assert(containsLine(capture, "first"), "first line matches");
  assert(containsLine(capture, "middle"), "middle line matches");
  assert(containsLine(capture, "last"), "last line matches");
  assert(!containsLine(capture, "absent"), "absent value does not match");
}

console.log("containsLine() matches the last line without a trailing newline...");
{
  // The other half of the old defect: with no trailing newline there is no
  // closing `\n` for the final line to anchor against.
  const capture = "first\nmiddle\nlast";

  assert(containsLine(capture, "first"), "first line matches (no trailing LF)");
  assert(containsLine(capture, "middle"), "middle line matches (no trailing LF)");
  assert(containsLine(capture, "last"), "last line matches (no trailing LF)");
}

console.log("containsLine() handles single-line captures...");
{
  // A single-line capture is simultaneously the first and the last line — the
  // old implementation could not match it in either form.
  assert(containsLine("only\n", "only"), "single line with trailing LF");
  assert(containsLine("only", "only"), "single line without trailing LF");
  assert(!containsLine("only\n", "onl"), "single line is not matched by a prefix");
}

console.log("containsLine() handles empty captures...");
{
  assertEqual(containsLine("", "anything"), false, "empty capture matches nothing");
  assertEqual(containsLine("", ""), false, "empty capture has no empty line either");
}

console.log("containsLine() treats a trailing terminator as a terminator...");
{
  // "a\n" is one line, not a line plus a following empty line. "a\n\n" is a
  // line followed by a genuinely blank line.
  assertEqual(containsLine("a\n", ""), false, "trailing LF does not create a blank line");
  assertEqual(containsLine("a\n\n", ""), true, "a real blank line matches the empty needle");
  assertEqual(containsLine("a\n\nb\n", ""), true, "an interior blank line matches");
  assertEqual(containsLine("a\r\n", ""), false, "trailing CRLF does not create a blank line");
}

// -- Whole-line matching, not substring matching ---------------------------------

console.log("containsLine() matches whole lines, never substrings...");
{
  // The point of the helper: a TAP-shaped "not ok" line must not satisfy an
  // assertion that the output contains "ok".
  const tap = "not ok 1 - fails\nnot ok 2 - fails\n";
  assertEqual(containsLine(tap, "ok"), false, '"ok" does not match "not ok 1 - fails"');
  assertEqual(containsLine("ok\nnot ok\n", "ok"), true, '"ok" matches its own line');

  assertEqual(containsLine("prefix-value\n", "value"), false, "suffix of a line does not match");
  assertEqual(containsLine("value-suffix\n", "value"), false, "prefix of a line does not match");
  assertEqual(containsLine("  value\n", "value"), false, "leading whitespace is significant");
  assertEqual(containsLine("value \n", "value"), false, "trailing whitespace is significant");

  // A multi-line needle can never be a single line, so it never matches.
  assertEqual(containsLine("a\nb\n", "a\nb"), false, "a multi-line needle matches no single line");
}

// -- CRLF tolerance ---------------------------------------------------------------

console.log("containsLine() tolerates CRLF captures...");
{
  // Pascal's WriteLn emits \r\n on Windows; the harnesses compare against LF
  // needles, so CRLF captures must match identically.
  const crlf = "first\r\nmiddle\r\nlast\r\n";
  assert(containsLine(crlf, "first"), "CRLF first line matches");
  assert(containsLine(crlf, "middle"), "CRLF middle line matches");
  assert(containsLine(crlf, "last"), "CRLF last line matches");
  assert(containsLine("first\r\nlast", "last"), "CRLF capture without trailing terminator");
  assert(containsLine("only\r", "only"), "capture ending in a bare CR");

  // Mixed endings occur when a Windows-built binary's output is concatenated
  // with harness-generated text.
  assert(containsLine("first\r\nmiddle\nlast\r\n", "middle"), "mixed CRLF/LF capture");

  // CRLF tolerance must not become substring tolerance.
  assertEqual(containsLine("not ok\r\n", "ok"), false, "CRLF capture is still whole-line");
}

// -- normalizeLineEndings() -------------------------------------------------------

console.log("normalizeLineEndings() folds captures to LF...");
{
  assertEqual(normalizeLineEndings("a\r\nb\r\n"), "a\nb\n", "CRLF string folds to LF");
  assertEqual(normalizeLineEndings("a\nb\n"), "a\nb\n", "LF string is unchanged");
  assertEqual(normalizeLineEndings(["a", "b"]), "a\nb\n", "array joins with a trailing LF");
  assertEqual(normalizeLineEndings([]), "", "empty array yields an empty string");

  // The two helpers compose: a normalized capture matches the same lines.
  assert(
    containsLine(normalizeLineEndings(["first", "middle", "last"]), "first"),
    "normalized array capture matches its first line",
  );
  assert(
    containsLine(normalizeLineEndings(["first", "middle", "last"]), "last"),
    "normalized array capture matches its last line",
  );
}

console.log(`\ntest-cli assertions: ${passed} assertions passed.`);
