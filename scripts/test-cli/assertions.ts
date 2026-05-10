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

export function assertSyntaxError(
  source: string,
  desc: string,
  extraArgs?: string[],
): void {
  const { exitCode, json } = runLoaderJson(source, extraArgs);
  if (exitCode === 0)
    throw new Error(`${desc} should fail, but exited 0`);
  if (json.ok !== false || json.error?.type !== "SyntaxError")
    throw new Error(
      `${desc} should be SyntaxError, got ok=${json.ok} type=${json.error?.type}`,
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
