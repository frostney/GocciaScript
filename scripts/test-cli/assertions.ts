/**
 * Shared output-matching helpers for the scripts/test-cli*.ts harnesses.
 *
 * Pascal's WriteLn writes \r\n on Windows, so substring matches against
 * \n<value>\n fail there unless \r is stripped first.
 */

/** Returns true when `value` appears on its own line in `s`, CRLF-tolerant. */
export const containsLine = (s: string, value: string): boolean =>
  s.replace(/\r/g, "").includes(`\n${value}\n`);

/**
 * Normalizes captured output to LF line endings. Accepts a raw string or
 * an array of lines (joined with `\n` and given a trailing `\n` when
 * non-empty), so callers can pass either the raw stdout or a JSON
 * `output` array uniformly.
 */
export function normalizeLineEndings(output: unknown): string {
  if (Array.isArray(output)) {
    const text = output.join("\n");
    return text.length > 0 ? `${text}\n` : "";
  }
  return String(output).replace(/\r\n/g, "\n");
}
