/**
 * Shared output-matching helpers for the scripts/test-cli*.ts harnesses.
 *
 * Pascal's WriteLn writes \r\n on Windows, so every matcher here has to treat
 * CRLF and LF captures identically.
 */

import { LOADER } from "./binaries";

/**
 * Returns true when `value` appears on its own line in `s`, CRLF-tolerant.
 *
 * Whole-line, not substring: `containsLine(out, "ok")` must not match a line
 * reading `not ok`. Every line counts, including the first and the last —
 * a capture is split into lines rather than searched for `\n<value>\n`, which
 * can never match a boundary line.
 *
 * A single trailing line terminator is a terminator, not a following empty
 * line, so `containsLine("a\n", "")` is false while `containsLine("a\n\n", "")`
 * is true. An empty capture has no lines at all.
 */
export const containsLine = (s: string, value: string): boolean => {
  if (s.length === 0) return false;
  const lines = s.split(/\r?\n/);
  // Drop the empty segment produced by a trailing terminator (not a line).
  if (lines[lines.length - 1] === "") lines.pop();
  // A capture ending in a bare CR (no LF) still ends that line there.
  return lines.some((line) => line.replace(/\r$/, "") === value);
};

export function runLoaderJson(
  source: string,
  extraArgs?: string[],
  opts?: { bin?: string; timeout?: number },
): { exitCode: number; json: any; stderr: string } {
  const hasOutputFlag = extraArgs?.some((a) => a.startsWith("--output="));
  const spawnOpts: {
    stdin: Uint8Array;
    stdout: "pipe";
    stderr: "pipe";
    timeout?: number;
  } = {
    stdin: new TextEncoder().encode(source),
    stdout: "pipe",
    stderr: "pipe",
  };
  if (opts?.timeout != null) spawnOpts.timeout = opts.timeout;
  const proc = Bun.spawnSync(
    [
      opts?.bin ?? LOADER,
      ...(hasOutputFlag ? [] : ["--output=json"]),
      ...(extraArgs ?? []),
    ],
    spawnOpts,
  );
  const stdout = proc.stdout.toString();
  const stderr = proc.stderr.toString();
  // A killed process has no exit code. With `timeout` set this is the signal
  // that the loader never terminated — report that directly instead of letting
  // it surface as an unparseable-JSON failure.
  if (proc.exitCode === null)
    throw new Error(
      `runLoaderJson: loader did not exit${
        opts?.timeout != null ? ` within ${opts.timeout}ms` : ""
      } (timed out)\nstderr: ${stderr}`,
    );
  let json: any;
  try {
    json = JSON.parse(stdout);
  } catch (e: any) {
    throw new Error(
      `runLoaderJson: failed to parse JSON (exitCode=${proc.exitCode}): ${e.message}\nstderr: ${stderr}\nstdout: ${stdout}`,
    );
  }
  return { exitCode: proc.exitCode, json, stderr };
}

/**
 * Asserts the loader rejects `source` with a positioned SyntaxError.
 *
 * `opts.timeout` bounds the run — required for sources that must be proven to
 * terminate, where a regression hangs the process instead of failing an
 * assertion. `opts.messageIncludes` pins the diagnostic so an unrelated syntax
 * error cannot satisfy the assertion, and `opts.line` pins the reported source
 * line for errors whose position is itself the thing under test.
 */
export function assertSyntaxError(
  source: string,
  desc: string,
  extraArgs?: string[],
  opts?: { timeout?: number; messageIncludes?: string; line?: number },
): void {
  const { exitCode, json } = runLoaderJson(source, extraArgs, opts);
  if (exitCode !== 1)
    throw new Error(`${desc} should exit 1, but exited ${exitCode}`);
  if (json.ok !== false || json.error?.type !== "SyntaxError")
    throw new Error(
      `${desc} should be SyntaxError, got ok=${json.ok} type=${json.error?.type}`,
    );
  if (
    typeof json.error.line !== "number" ||
    typeof json.error.column !== "number"
  )
    throw new Error(
      `${desc} should include numeric line and column, got line=${json.error.line} column=${json.error.column}`,
    );
  if (
    opts?.messageIncludes != null &&
    !String(json.error.message).includes(opts.messageIncludes)
  )
    throw new Error(
      `${desc} should mention ${JSON.stringify(opts.messageIncludes)}, got ${JSON.stringify(json.error.message)}`,
    );
  if (opts?.line != null && json.error.line !== opts.line)
    throw new Error(
      `${desc} should be reported on line ${opts.line}, got line ${json.error.line}`,
    );
}

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
