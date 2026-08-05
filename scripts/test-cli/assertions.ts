/**
 * Shared output-matching helpers for the scripts/test-cli*.ts harnesses.
 *
 * Pascal's WriteLn writes \r\n on Windows, so substring matches against
 * \n<value>\n fail there unless \r is stripped first.
 */

import { LOADER } from "./binaries";

/** Returns true when `value` appears on its own line in `s`, CRLF-tolerant. */
export const containsLine = (s: string, value: string): boolean =>
  s.replace(/\r/g, "").includes(`\n${value}\n`);

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
 * error cannot satisfy the assertion.
 */
export function assertSyntaxError(
  source: string,
  desc: string,
  extraArgs?: string[],
  opts?: { timeout?: number; messageIncludes?: string },
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
