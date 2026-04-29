import "server-only";

import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path, { basename } from "node:path";
import { NextResponse } from "next/server";
import {
  type GocciaToolInput,
  MAX_GOCCIA_CODE_BYTES,
  MAX_GOCCIA_TOOL_REQUEST_BYTES,
  validateGocciaToolInput,
} from "@/lib/goccia-tool-schema";
import {
  captureServerEvent,
  captureServerException,
} from "@/lib/posthog-server";
import { clientIp, rateLimit } from "@/lib/rate-limit";

const TIMEOUT_MS = 5_000;
const MAX_OUTPUT_BYTES = 256 * 1024;
const MAX_BODY_BYTES = MAX_GOCCIA_TOOL_REQUEST_BYTES;
const MAX_MEMORY_BYTES = 32 * 1024 * 1024;
const MAX_INSTRUCTIONS = 50_000_000;
const STACK_SIZE = 2_000;
const ALLOWED_HOSTS = ["icanhazdadjoke.com"];

type GocciaRequestBody = {
  code: string;
  mode?: "interpreted" | "bytecode";
  asi?: boolean;
  compatVar?: boolean;
  compatFunction?: boolean;
};

type EndpointConfig = {
  kind: "execute" | "test";
  path: "/api/execute" | "/api/test";
  eventPrefix: "api_execute" | "api_test";
  rateLimitPrefix: "execute" | "test";
  binaryEnvVar: "GOCCIA_BINARY" | "GOCCIA_TEST_BINARY";
};

const EXECUTE_ENDPOINT: EndpointConfig = {
  kind: "execute",
  path: "/api/execute",
  eventPrefix: "api_execute",
  rateLimitPrefix: "execute",
  binaryEnvVar: "GOCCIA_BINARY",
};

const TEST_ENDPOINT: EndpointConfig = {
  kind: "test",
  path: "/api/test",
  eventPrefix: "api_test",
  rateLimitPrefix: "test",
  binaryEnvVar: "GOCCIA_TEST_BINARY",
};

/** All early-error responses (rate limit, bad body, oversize) follow this
 *  same shape, so the client only ever has to read `error.message` /
 *  `error.code`. The runner's own structured `error` (different schema)
 *  lives on the success-path body alongside runner fields and is not affected.
 */
type TransportError = {
  message: string;
  code:
    | "RATE_LIMIT"
    | "INVALID_INPUT"
    | "INVALID_JSON"
    | "MISSING_CODE"
    | "CODE_TOO_LARGE"
    | "SPAWN_FAILED"
    | "ABORTED";
};

type TimingJson = {
  lex_ms?: number;
  parse_ms?: number;
  compile_ms?: number;
  exec_ms?: number;
  total_ms?: number;
  lex_ns?: number;
  parse_ns?: number;
  compile_ns?: number;
  exec_ns?: number;
  total_ns?: number;
};

type Invocation = {
  binary: string;
  args: string[];
  stdin?: string;
  resultFile?: string;
  privateDirectory?: string;
  privateFileName?: string;
  publicFileName?: string;
  cleanup?: () => Promise<void>;
};

function runtimeTelemetryProperties(
  body: GocciaRequestBody,
  asi: boolean,
  compatVar: boolean,
  compatFunction: boolean,
  codeBytes?: number,
): Record<string, unknown> {
  return {
    mode: body.mode === "bytecode" ? "bytecode" : "interpreted",
    asi,
    compatVar,
    compatFunction,
    ...(codeBytes === undefined ? {} : { codeBytes }),
  };
}

function transportError(
  err: TransportError,
  init?: ResponseInit & { extra?: Record<string, unknown> },
): Response {
  const { extra, ...rest } = init ?? {};
  return NextResponse.json({ error: err, ...(extra ?? {}) }, rest);
}

/** Read `req.body` as a UTF-8 string, rejecting once the cumulative byte
 *  count exceeds `cap`. Used in place of `req.text()` / `req.json()` so
 *  oversized payloads are short-circuited mid-stream rather than fully
 *  buffered. Returns `null` on overflow.
 */
async function readBodyWithCap(
  req: Request,
  cap: number,
): Promise<string | null> {
  if (!req.body) return "";
  const reader = req.body.getReader();
  const chunks: Uint8Array[] = [];
  let total = 0;
  while (true) {
    const { value, done } = await reader.read();
    if (done) break;
    if (!value) continue;
    total += value.byteLength;
    if (total > cap) {
      try {
        await reader.cancel();
      } catch {}
      return null;
    }
    chunks.push(value);
  }
  return Buffer.concat(chunks).toString("utf8");
}

// Resolution order, first existing path wins:
//   1. endpoint-specific env var (deploy / debug override)
//   2. `../build/<binary>` - the locally built engine, so `bun run dev`
//      always uses whatever the developer just compiled with `./build.pas`
//   3. `vendor/<binary>` - fetched at `prebuild` time for Vercel deploys
function resolveBinaryPath(config: EndpointConfig): string {
  const override = process.env[config.binaryEnvVar];
  if (override) return override;

  const localBinary =
    config.kind === "execute"
      ? path.join(
          /* turbopackIgnore: true */ process.cwd(),
          "..",
          "build",
          "GocciaScriptLoader",
        )
      : path.join(
          /* turbopackIgnore: true */ process.cwd(),
          "..",
          "build",
          "GocciaTestRunner",
        );
  const vendorBinary =
    config.kind === "execute"
      ? path.join(process.cwd(), "vendor", "GocciaScriptLoader")
      : path.join(process.cwd(), "vendor", "GocciaTestRunner");

  const candidates = [localBinary, vendorBinary];
  for (const candidate of candidates) {
    if (existsSync(/* turbopackIgnore: true */ candidate)) return candidate;
  }
  return candidates[candidates.length - 1];
}

function buildEngineArgs(
  body: GocciaRequestBody,
  asi: boolean,
  compatVar: boolean,
  compatFunction: boolean,
): string[] {
  const args = [
    `--timeout=${TIMEOUT_MS}`,
    `--max-memory=${MAX_MEMORY_BYTES}`,
    `--max-instructions=${MAX_INSTRUCTIONS}`,
    `--stack-size=${STACK_SIZE}`,
    ...ALLOWED_HOSTS.flatMap((host) => ["--allowed-host", host]),
  ];
  if (asi) args.unshift("--asi");
  if (compatVar) args.push("--compat-var");
  if (compatFunction) args.push("--compat-function");
  if (body.mode === "bytecode") args.push("--mode=bytecode");
  return args;
}

async function prepareInvocation(
  config: EndpointConfig,
  body: GocciaRequestBody,
  code: string,
  asi: boolean,
  compatVar: boolean,
  compatFunction: boolean,
): Promise<Invocation> {
  const binary = resolveBinaryPath(config);
  const engineArgs = buildEngineArgs(body, asi, compatVar, compatFunction);

  if (config.kind === "execute") {
    return {
      binary,
      args: ["--output=json", ...engineArgs],
      stdin: code,
    };
  }

  const dir = await mkdtemp(
    path.join(/* turbopackIgnore: true */ tmpdir(), "goccia-test-api-"),
  );
  const testFile = path.join(dir, "inline-test.js");
  const resultFile = path.join(dir, "results.json");
  await writeFile(/* turbopackIgnore: true */ testFile, code, "utf8");

  return {
    binary,
    args: [
      testFile,
      "--no-progress",
      "--no-results",
      `--output=${resultFile}`,
      ...engineArgs,
    ],
    resultFile,
    privateDirectory: dir,
    privateFileName: testFile,
    publicFileName: "<inline-test.js>",
    cleanup: () =>
      rm(/* turbopackIgnore: true */ dir, { recursive: true, force: true }),
  };
}

function asRecord(value: unknown): Record<string, unknown> | null {
  if (value && typeof value === "object" && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }
  return null;
}

function optionalString(value: unknown): string | undefined {
  return typeof value === "string" && value.length > 0 ? value : undefined;
}

function outputToText(value: unknown): string {
  if (typeof value === "string") return value;
  if (Array.isArray(value)) {
    return value
      .map((line) => (typeof line === "string" ? line : String(line)))
      .join("\n");
  }
  return "";
}

function numberField(
  record: Record<string, unknown>,
  key: keyof TimingJson,
): number | undefined {
  const value = record[key];
  return typeof value === "number" && Number.isFinite(value)
    ? value
    : undefined;
}

function normalizeTiming(value: unknown): TimingJson | null {
  const record = asRecord(value);
  if (!record) return null;

  const timing: TimingJson = {};
  for (const key of [
    "lex_ms",
    "parse_ms",
    "compile_ms",
    "exec_ms",
    "total_ms",
    "lex_ns",
    "parse_ns",
    "compile_ns",
    "exec_ns",
    "total_ns",
  ] as const) {
    const field = numberField(record, key);
    if (field !== undefined) timing[key] = field;
  }

  if (timing.lex_ms === undefined && timing.lex_ns !== undefined) {
    timing.lex_ms = timing.lex_ns / 1_000_000;
  }
  if (timing.parse_ms === undefined && timing.parse_ns !== undefined) {
    timing.parse_ms = timing.parse_ns / 1_000_000;
  }
  if (timing.compile_ms === undefined && timing.compile_ns !== undefined) {
    timing.compile_ms = timing.compile_ns / 1_000_000;
  }
  if (timing.exec_ms === undefined && timing.exec_ns !== undefined) {
    timing.exec_ms = timing.exec_ns / 1_000_000;
  }
  if (timing.total_ms === undefined && timing.total_ns !== undefined) {
    timing.total_ms = timing.total_ns / 1_000_000;
  }

  return Object.keys(timing).length > 0 ? timing : null;
}

function firstFileResult(record: Record<string, unknown> | null): unknown {
  const files = record?.files;
  if (!Array.isArray(files)) return undefined;
  const first = asRecord(files[0]);
  return first?.result;
}

function errorName(error: unknown): string | null {
  const record = asRecord(error);
  if (!record) return null;
  return optionalString(record.name) ?? optionalString(record.type) ?? null;
}

function numericProperty(
  record: Record<string, unknown> | null,
  key: string,
): number | null {
  const value = record?.[key];
  return typeof value === "number" && Number.isFinite(value) ? value : null;
}

function replaceAllText(value: string, search: string, replacement: string) {
  return value.split(search).join(replacement);
}

function sanitizeInvocationString(
  value: string,
  invocation: Invocation,
): string {
  let sanitized = value;
  if (invocation.privateFileName && invocation.publicFileName) {
    sanitized = replaceAllText(
      sanitized,
      invocation.privateFileName,
      invocation.publicFileName,
    );
  }
  if (invocation.privateDirectory) {
    sanitized = replaceAllText(sanitized, invocation.privateDirectory, "<tmp>");
  }
  return sanitized;
}

function sanitizeInvocationValue(
  value: unknown,
  invocation: Invocation,
): unknown {
  if (typeof value === "string") {
    return sanitizeInvocationString(value, invocation);
  }
  if (Array.isArray(value)) {
    return value.map((item) => sanitizeInvocationValue(item, invocation));
  }
  const record = asRecord(value);
  if (!record) return value;
  return Object.fromEntries(
    Object.entries(record).map(([key, item]) => [
      key,
      sanitizeInvocationValue(item, invocation),
    ]),
  );
}

async function readInvocationResult(
  invocation: Invocation,
  stdoutText: string,
): Promise<unknown | null> {
  const payload = invocation.resultFile
    ? await readFile(/* turbopackIgnore: true */ invocation.resultFile, "utf8")
    : stdoutText;
  try {
    return JSON.parse(payload);
  } catch {
    return null;
  }
}

function buildExecuteResponseBody(
  parsed: unknown | null,
  stdoutText: string,
  stderrText: string,
  exitCode: number | null,
  signal: NodeJS.Signals | null,
  truncated: boolean,
): Record<string, unknown> {
  const record = asRecord(parsed);
  const timing = normalizeTiming(record?.timing);
  return {
    ok: typeof record?.ok === "boolean" ? record.ok : false,
    value: record?.value ?? firstFileResult(record) ?? null,
    output: outputToText(record?.output),
    error: record?.error ?? null,
    timing,
    exitCode,
    signal,
    truncated,
    stderr: stderrText || optionalString(record?.stderr),
    rawStdout: parsed ? undefined : stdoutText || undefined,
  };
}

function buildTestResponseBody(
  parsed: unknown | null,
  stdoutText: string,
  stderrText: string,
  exitCode: number | null,
  signal: NodeJS.Signals | null,
  truncated: boolean,
): Record<string, unknown> {
  const record = asRecord(parsed);
  if (!record) {
    return {
      ok: false,
      error: null,
      timing: null,
      exitCode,
      signal,
      truncated,
      stderr: stderrText || undefined,
      rawStdout: stdoutText || undefined,
    };
  }

  return {
    ...record,
    output: outputToText(record.output),
    stdout: stdoutText || optionalString(record.stdout) || "",
    stderr: stderrText || optionalString(record.stderr),
    timing: normalizeTiming(record.timing),
    exitCode,
    signal,
    truncated,
  };
}

function buildResponseBody(
  config: EndpointConfig,
  invocation: Invocation,
  parsed: unknown | null,
  stdoutText: string,
  stderrText: string,
  exitCode: number | null,
  signal: NodeJS.Signals | null,
  truncated: boolean,
): Record<string, unknown> {
  const responseParsed =
    config.kind === "test"
      ? sanitizeInvocationValue(parsed, invocation)
      : parsed;
  const responseStdoutText =
    config.kind === "test"
      ? sanitizeInvocationString(stdoutText, invocation)
      : stdoutText;
  const responseStderrText =
    config.kind === "test"
      ? sanitizeInvocationString(stderrText, invocation)
      : stderrText;

  if (config.kind === "execute") {
    return buildExecuteResponseBody(
      responseParsed,
      responseStdoutText,
      responseStderrText,
      exitCode,
      signal,
      truncated,
    );
  }

  return buildTestResponseBody(
    responseParsed,
    responseStdoutText,
    responseStderrText,
    exitCode,
    signal,
    truncated,
  );
}

async function runHandler(
  req: Request,
  ip: string,
  distinctId: string,
  config: EndpointConfig,
): Promise<Response> {
  const rl = rateLimit(`${config.rateLimitPrefix}:${ip}`);
  if (!rl.ok) {
    const retryAfter = Math.max(1, Math.ceil((rl.resetAt - Date.now()) / 1000));
    captureServerEvent(`${config.eventPrefix}_rate_limited`, {
      distinctId,
      path: config.path,
      properties: { retryAfter, limit: rl.limit },
    });
    return transportError(
      { message: "rate limit exceeded", code: "RATE_LIMIT" },
      {
        status: 429,
        headers: {
          "Retry-After": String(retryAfter),
          "X-RateLimit-Limit": String(rl.limit),
          "X-RateLimit-Remaining": "0",
          "X-RateLimit-Reset": String(Math.ceil(rl.resetAt / 1000)),
        },
        extra: { retryAfter },
      },
    );
  }

  // Reject oversized payloads BEFORE parsing. `req.json()` would buffer the
  // entire body first, which is a DoS vector for arbitrarily large requests.
  const contentLength = req.headers.get("content-length");
  if (contentLength !== null) {
    const declared = Number(contentLength);
    if (Number.isFinite(declared) && declared > MAX_BODY_BYTES) {
      captureServerEvent(`${config.eventPrefix}_body_too_large`, {
        distinctId,
        path: config.path,
        properties: { declared, source: "content-length" },
      });
      return transportError(
        {
          message: `request body exceeds ${MAX_BODY_BYTES} bytes`,
          code: "CODE_TOO_LARGE",
        },
        { status: 413 },
      );
    }
  }

  const rawBody = await readBodyWithCap(req, MAX_BODY_BYTES);
  if (rawBody === null) {
    captureServerEvent(`${config.eventPrefix}_body_too_large`, {
      distinctId,
      path: config.path,
      properties: { source: "stream" },
    });
    return transportError(
      {
        message: `request body exceeds ${MAX_BODY_BYTES} bytes`,
        code: "CODE_TOO_LARGE",
      },
      { status: 413 },
    );
  }

  let rawPayload: GocciaToolInput;
  try {
    rawPayload = JSON.parse(rawBody) as GocciaToolInput;
  } catch {
    captureServerEvent(`${config.eventPrefix}_invalid_json`, {
      distinctId,
      path: config.path,
    });
    return transportError(
      { message: "invalid JSON body", code: "INVALID_JSON" },
      { status: 400 },
    );
  }

  const payload = validateGocciaToolInput(rawPayload);
  if (!payload.ok) {
    const status = payload.error.code === "CODE_TOO_LARGE" ? 413 : 400;
    if (payload.error.code === "CODE_TOO_LARGE") {
      captureServerEvent(`${config.eventPrefix}_code_too_large`, {
        distinctId,
        path: config.path,
        properties: { source: "schema" },
      });
    }
    return transportError(
      {
        message: payload.error.message,
        code:
          payload.error.code === "CODE_TOO_LARGE"
            ? "CODE_TOO_LARGE"
            : payload.error.code === "MISSING_CODE"
              ? "MISSING_CODE"
              : "INVALID_INPUT",
      },
      { status },
    );
  }

  const body = payload.value;
  const code = body.code;
  // UTF-16 code-unit count (`code.length`) under-counts for non-BMP source;
  // measure the actual bytes that will hit stdin or the temp test file.
  const codeBytes = Buffer.byteLength(code, "utf8");
  if (codeBytes > MAX_GOCCIA_CODE_BYTES) {
    captureServerEvent(`${config.eventPrefix}_code_too_large`, {
      distinctId,
      path: config.path,
      properties: { codeBytes },
    });
    return transportError(
      {
        message: `code exceeds ${MAX_GOCCIA_CODE_BYTES} bytes`,
        code: "CODE_TOO_LARGE",
      },
      { status: 413 },
    );
  }

  const { asi, compatVar, compatFunction } = body;
  const invocation = await prepareInvocation(
    config,
    body,
    code,
    asi,
    compatVar,
    compatFunction,
  );
  const startedAt = Date.now();

  return await new Promise<Response>((resolve) => {
    let resolved = false;
    let cleaned = false;
    let abortHandler: (() => void) | null = null;

    const cleanup = async () => {
      if (cleaned) return;
      cleaned = true;
      await invocation.cleanup?.();
    };

    const finish = (res: Response) => {
      if (resolved) return;
      resolved = true;
      if (abortHandler) {
        try {
          req.signal.removeEventListener("abort", abortHandler);
        } catch {}
        abortHandler = null;
      }
      resolve(res);
    };

    // Build a minimal allowlisted env so we don't forward server secrets
    // (DB URLs, API tokens, deploy-platform vars, etc.) into the sandboxed
    // binaries. Only variables the binaries themselves need are passed through.
    const ALLOWED_ENV = [
      "PATH",
      "HOME",
      "TMPDIR",
      "LANG",
      "LC_ALL",
      "TZ",
      "NODE_ENV",
    ];
    const childEnv: Record<string, string | undefined> = { NO_COLOR: "1" };
    for (const key of ALLOWED_ENV) {
      const value = process.env[key];
      if (typeof value === "string") childEnv[key] = value;
    }

    const child = spawn(invocation.binary, invocation.args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: childEnv as NodeJS.ProcessEnv,
    });

    abortHandler = () => {
      child.kill("SIGKILL");
      captureServerEvent(`${config.eventPrefix}_aborted`, {
        distinctId,
        path: config.path,
        properties: {
          elapsedMs: Date.now() - startedAt,
          ...runtimeTelemetryProperties(
            body,
            asi,
            compatVar,
            compatFunction,
            codeBytes,
          ),
        },
      });
      finish(
        transportError(
          { message: "client disconnected", code: "ABORTED" },
          { status: 499 },
        ),
      );
    };
    if (req.signal.aborted) {
      abortHandler();
      return;
    }
    req.signal.addEventListener("abort", abortHandler, { once: true });

    // Accumulate raw bytes per stream. Counting `string.length` would count
    // UTF-16 code units, not bytes, so multibyte UTF-8 output could exceed
    // MAX_OUTPUT_BYTES before we noticed.
    const stdoutChunks: Buffer[] = [];
    const stderrChunks: Buffer[] = [];
    let stdoutBytes = 0;
    let stderrBytes = 0;
    let truncated = false;

    const killTimer = setTimeout(() => {
      truncated = true;
      child.kill("SIGKILL");
    }, TIMEOUT_MS + 1_500);

    child.stdout.on("data", (chunk: Buffer) => {
      if (stdoutBytes + chunk.length > MAX_OUTPUT_BYTES) {
        const remaining = MAX_OUTPUT_BYTES - stdoutBytes;
        if (remaining > 0) {
          stdoutChunks.push(chunk.subarray(0, remaining));
          stdoutBytes += remaining;
        }
        truncated = true;
        child.kill("SIGKILL");
      } else {
        stdoutChunks.push(chunk);
        stdoutBytes += chunk.length;
      }
    });

    child.stderr.on("data", (chunk: Buffer) => {
      if (stderrBytes + chunk.length > MAX_OUTPUT_BYTES) {
        const remaining = MAX_OUTPUT_BYTES - stderrBytes;
        if (remaining > 0) {
          stderrChunks.push(chunk.subarray(0, remaining));
          stderrBytes += remaining;
        }
      } else {
        stderrChunks.push(chunk);
        stderrBytes += chunk.length;
      }
    });

    child.on("error", async (err) => {
      clearTimeout(killTimer);
      console.error(
        `[${config.path}] spawn failed for binary: ${invocation.binary} - ${err.message}`,
      );
      captureServerException(err, {
        distinctId,
        path: config.path,
        extra: {
          stage: "spawn",
          binary: invocation.binary,
          binaryName: basename(invocation.binary),
          ...runtimeTelemetryProperties(body, asi, compatVar, compatFunction),
        },
      });
      captureServerEvent(`${config.eventPrefix}_spawn_failed`, {
        distinctId,
        path: config.path,
        properties: {
          binaryName: basename(invocation.binary),
          message: err.message,
          ...runtimeTelemetryProperties(body, asi, compatVar, compatFunction),
        },
      });
      await cleanup();
      finish(
        transportError(
          {
            message: `spawn failed: ${err.message}`,
            code: "SPAWN_FAILED",
          },
          { status: 500, extra: { binary: basename(invocation.binary) } },
        ),
      );
    });

    child.on("close", async (exitCode, signal) => {
      clearTimeout(killTimer);
      const stdoutText = Buffer.concat(stdoutChunks).toString("utf8");
      const stderrText = Buffer.concat(stderrChunks).toString("utf8");
      let parsed: unknown | null = null;
      try {
        parsed = await readInvocationResult(invocation, stdoutText);
      } catch {
        parsed = null;
      }
      await cleanup();

      const record = asRecord(parsed);
      const timing = normalizeTiming(record?.timing);
      captureServerEvent(`${config.eventPrefix}_completed`, {
        distinctId,
        path: config.path,
        properties: {
          ok: typeof record?.ok === "boolean" ? record.ok : false,
          parsed: parsed !== null,
          exitCode,
          signal,
          truncated,
          ...runtimeTelemetryProperties(
            body,
            asi,
            compatVar,
            compatFunction,
            codeBytes,
          ),
          stdoutBytes,
          stderrBytes,
          elapsedMs: Date.now() - startedAt,
          runnerTotalMs: timing?.total_ms ?? null,
          errorName: errorName(record?.error),
          totalFiles: numericProperty(record, "totalFiles"),
          totalTests: numericProperty(record, "totalTests"),
          failed: numericProperty(record, "failed"),
        },
      });

      finish(
        NextResponse.json(
          buildResponseBody(
            config,
            invocation,
            parsed,
            stdoutText,
            stderrText,
            exitCode,
            signal,
            truncated,
          ),
          {
            headers: {
              "X-RateLimit-Limit": String(rl.limit),
              "X-RateLimit-Remaining": String(rl.remaining),
              "X-RateLimit-Reset": String(Math.ceil(rl.resetAt / 1000)),
            },
          },
        ),
      );
    });

    // The child can close stdin before we finish writing (early exit on parse
    // error, OOM, killed by our timeout, etc.). Without a listener Node treats
    // the EPIPE as unhandled and crashes the request worker.
    child.stdin.on("error", (err: NodeJS.ErrnoException) => {
      if (err && err.code !== "EPIPE") {
        console.error(`[${config.path}] stdin error:`, err);
      }
    });

    if (invocation.stdin !== undefined) {
      child.stdin.write(invocation.stdin);
    }
    child.stdin.end();
  });
}

async function handleGocciaRequest(
  req: Request,
  config: EndpointConfig,
): Promise<Response> {
  const ip = clientIp(req.headers);
  const distinctId = `ip:${ip}`;
  try {
    return await runHandler(req, ip, distinctId, config);
  } catch (err) {
    console.error(`[${config.path}] unhandled exception:`, err);
    captureServerException(err, {
      distinctId,
      path: config.path,
      extra: {
        ua: req.headers.get("user-agent") ?? undefined,
        method: req.method,
      },
    });
    return transportError(
      { message: "internal error", code: "SPAWN_FAILED" },
      { status: 500 },
    );
  }
}

export function handleExecuteRequest(req: Request): Promise<Response> {
  return handleGocciaRequest(req, EXECUTE_ENDPOINT);
}

export function handleTestRequest(req: Request): Promise<Response> {
  return handleGocciaRequest(req, TEST_ENDPOINT);
}
