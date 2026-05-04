/**
 * Shared helpers for the scripts/test-cli*.ts smoke harnesses.
 */

/**
 * Returns true when `value` appears on its own line in `s`.
 *
 * Pascal's WriteLn writes \r\n on Windows, so the literal substring
 * `\n${value}\n` is never present in captured stdout — the bytes are
 * `\r\n${value}\r\n`. Strip \r before matching so the same assertion
 * works on every platform.
 */
export const containsLine = (s: string, value: string): boolean =>
  s.replace(/\r/g, "").includes(`\n${value}\n`);
