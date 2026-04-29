import "server-only";

import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import path, { basename } from "node:path";
import { NextResponse } from "next/server";
import {
  MAX_GOCCIA_CODE_BYTES,
  MAX_GOCCIA_TOOL_REQUEST_BYTES,
  validateGocciaToolInput,
} from "@/lib/goccia-tool-schema";
import {
  captureServerEvent,
  captureServerException,
} from "@/lib/posthog-server";
import { clientIp, rateLimit } from "@/lib/rate-limit";
import {
  isResponseCacheable,
  responseCacheGet,
  responseCacheKey,
  responseCacheSet,
} from "@/lib/response-cache";
import {
  findVersion,
  isFlagSupported,
  type VendorFeatureSet,
} from "@/lib/vendor-manifest";
import { getVendorManifest } from "@/lib/vendor-manifest-server";

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
  version?: string;
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
    | "ABORTED"
    | "UNKNOWN_VERSION";
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
  resolvedVersion: string,
  codeBytes?: number,
): Record<string, unknown> {
  return {
    mode: body.mode === "bytecode" ? "bytecode" : "interpreted",
    asi,
    compatVar,
    compatFunction,
    version: resolvedVersion,
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

type ResolvedBinary =
  | {
      ok: true;
      path: string;
      resolvedVersion: string;
      features: VendorFeatureSet | undefined;
    }
  | { ok: false; error: TransportError };

// Resolution order, first existing path wins:
//   1. endpoint-specific env var (deploy / debug override)
//   2. `vendor/<entry.binaries.*>` for the requested release tag — populated
//      at `prebuild` time by `scripts/fetch-binaries.ts` and described in
//      `vendor/manifest.json`. Per-version paths preserve archive-side
//      filenames so pre-0.7.0's `ScriptLoader`/`TestRunner` and post-0.7.0's
//      `GocciaScriptLoader`/`GocciaTestRunner` both resolve cleanly.
//   3. dev-only fallback to `../build/<binary>` — the locally compiled engine
//      `./build.pas` produced. Disabled in production so an unknown version
//      can't silently fall through to the wrong binary on Vercel.
function resolveBinaryPath(
  config: EndpointConfig,
  requestedVersion: string,
): ResolvedBinary {
  const override = process.env[config.binaryEnvVar];
  if (override) {
    // Override path bypasses the manifest entirely — caller assumes the
    // binary is current. Skipping feature filtering preserves the legacy
    // "I know what I'm doing" debug semantics.
    return {
      ok: true,
      path: override,
      resolvedVersion: requestedVersion,
      features: undefined,
    };
  }

  const manifest = getVendorManifest();
  const entry = findVersion(manifest, requestedVersion);
  if (entry) {
    const rel =
      config.kind === "execute"
        ? entry.binaries.loader
        : entry.binaries.testRunner;
    const abs = path.join(process.cwd(), "vendor", rel);
    if (existsSync(/* turbopackIgnore: true */ abs)) {
      return {
        ok: true,
        path: abs,
        resolvedVersion: entry.tag,
        features: entry.features,
      };
    }
  }

  if (process.env.NODE_ENV !== "production") {
    const localName =
      config.kind === "execute" ? "GocciaScriptLoader" : "GocciaTestRunner";
    const localPath = path.join(
      /* turbopackIgnore: true */ process.cwd(),
      "..",
      "build",
      localName,
    );
    if (existsSync(/* turbopackIgnore: true */ localPath)) {
      // Locally compiled engine: no probe, no filtering — the developer is
      // running whatever they just built.
      return {
        ok: true,
        path: localPath,
        resolvedVersion: "local",
        features: undefined,
      };
    }
  }

  return {
    ok: false,
    error: {
      message: `unknown version: ${requestedVersion}`,
      code: "UNKNOWN_VERSION",
    },
  };
}

/** Build the per-request arg list. Sandbox/infrastructure flags are
 *  filtered against the binary's probed `features` so older engines that
 *  don't recognize them (`--max-memory`, `--allowed-host`, …) still execute
 *  instead of erroring on first unknown-option.
 *
 *  User-toggled compat flags (`--compat-var`, `--compat-function`) are an
 *  exception: when the user explicitly opted in, we want the engine's own
 *  "Unknown option" error to surface so they know the toggle had no effect.
 *  Silently dropping them would be misleading. */
function buildEngineArgs(
  body: GocciaRequestBody,
  asi: boolean,
  compatVar: boolean,
  compatFunction: boolean,
  features: VendorFeatureSet | undefined,
  kind: "loader" | "testRunner",
): string[] {
  const args: string[] = [];
  const accept = (arg: string) => {
    if (isFlagSupported(features, arg, kind)) args.push(arg);
  };
  accept(`--timeout=${TIMEOUT_MS}`);
  accept(`--max-memory=${MAX_MEMORY_BYTES}`);
  accept(`--max-instructions=${MAX_INSTRUCTIONS}`);
  accept(`--stack-size=${STACK_SIZE}`);
  if (isFlagSupported(features, "--allowed-host", kind)) {
    for (const host of ALLOWED_HOSTS) {
      args.push("--allowed-host", host);
    }
  }
  if (asi && isFlagSupported(features, "--asi", kind)) {
    args.unshift("--asi");
  }
  // Compat flags pass through unconditionally — the engine errors when it
  // doesn't recognize them, and that's the desired UX (the user toggled it).
  if (compatVar) args.push("--compat-var");
  if (compatFunction) args.push("--compat-function");
  if (body.mode === "bytecode" && isFlagSupported(features, "--mode", kind)) {
    args.push("--mode=bytecode");
  }
  return args;
}

async function prepareInvocation(
  config: EndpointConfig,
  body: GocciaRequestBody,
  code: string,
  asi: boolean,
  compatVar: boolean,
  compatFunction: boolean,
  binary: string,
  features: VendorFeatureSet | undefined,
): Promise<Invocation> {
  const kind = config.kind === "execute" ? "loader" : "testRunner";
  const engineArgs = buildEngineArgs(
    body,
    asi,
    compatVar,
    compatFunction,
    features,
    kind,
  );

  if (config.kind === "execute") {
    const args: string[] = [];
    if (isFlagSupported(features, "--output", "loader")) {
      args.push("--output=json");
    }
    args.push(...engineArgs);
    return { binary, args, stdin: code };
  }

  const dir = await mkdtemp(
    path.join(/* turbopackIgnore: true */ tmpdir(), "goccia-test-api-"),
  );
  const testFile = path.join(dir, "inline-test.js");
  const resultFile = path.join(dir, "results.json");
  await writeFile(/* turbopackIgnore: true */ testFile, code, "utf8");

  // Build the test-runner arg list defensively: each flag is gated against
  // the binary's probed feature set. When `--output=<path>` isn't supported
  // we fall back to parsing stdout (`readInvocationResult` already handles
  // an absent `resultFile`).
  const args: string[] = [testFile];
  if (isFlagSupported(features, "--no-progress", "testRunner")) {
    args.push("--no-progress");
  }
  if (isFlagSupported(features, "--no-results", "testRunner")) {
    args.push("--no-results");
  }
  let resolvedResultFile: string | undefined;
  if (isFlagSupported(features, "--output", "testRunner")) {
    args.push(`--output=${resultFile}`);
    resolvedResultFile = resultFile;
  }
  args.push(...engineArgs);

  return {
    binary,
    args,
    resultFile: resolvedResultFile,
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

  let rawPayload: unknown;
  try {
    rawPayload = JSON.parse(rawBody);
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
        code: payload.error.code,
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
  const requestedVersion = body.version ?? getVendorManifest().defaultVersion;
  const resolved = resolveBinaryPath(config, requestedVersion);
  if (!resolved.ok) {
    captureServerEvent(`${config.eventPrefix}_unknown_version`, {
      distinctId,
      path: config.path,
      properties: { requestedVersion },
    });
    return transportError(resolved.error, { status: 400 });
  }
  const resolvedVersion = resolved.resolvedVersion;

  // Same input -> same response within the cache TTL. A client mashing the
  // "Run" button on the playground can short-circuit here without spawning
  // a fresh runner subprocess. Rate limiting still ran above, so cache hits
  // continue to count against the per-IP budget — the cache only avoids
  // server CPU, not bandwidth abuse.
  //
  // The cache key uses `resolvedVersion`, not the request's `version`, so a
  // request that omits `version` (defaulting to `manifest.defaultVersion`)
  // and a request that names that same version explicitly share a single
  // cache entry. Different versions get isolated entries — switching the
  // dropdown to nightly or v0.6.1 always re-spawns until that version's
  // cache fills.
  const mode: "interpreted" | "bytecode" =
    body.mode === "bytecode" ? "bytecode" : "interpreted";
  const cacheKey = responseCacheKey({
    kind: config.kind,
    code,
    mode,
    asi,
    compatVar,
    compatFunction,
    version: resolvedVersion,
  });
  const cached = responseCacheGet(cacheKey);
  if (cached !== null) {
    captureServerEvent(`${config.eventPrefix}_cache_hit`, {
      distinctId,
      path: config.path,
      properties: runtimeTelemetryProperties(
        body,
        asi,
        compatVar,
        compatFunction,
        resolvedVersion,
        codeBytes,
      ),
    });
    return NextResponse.json(cached, {
      headers: {
        "X-Cache": "HIT",
        "X-RateLimit-Limit": String(rl.limit),
        "X-RateLimit-Remaining": String(rl.remaining),
        "X-RateLimit-Reset": String(Math.ceil(rl.resetAt / 1000)),
      },
    });
  }

  const invocation = await prepareInvocation(
    config,
    body,
    code,
    asi,
    compatVar,
    compatFunction,
    resolved.path,
    resolved.features,
  );
  const startedAt = Date.now();

  return await new Promise<Response>((resolve) => {
    // Renamed from `resolved` to avoid shadowing the outer
    // `const resolved = resolveBinaryPath(...)` from `runHandler`.
    let responseSent = false;
    let cleaned = false;
    let abortHandler: (() => void) | null = null;

    const cleanup = async () => {
      if (cleaned) return;
      cleaned = true;
      await invocation.cleanup?.();
    };

    const finish = (res: Response) => {
      if (responseSent) return;
      responseSent = true;
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
            resolvedVersion,
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
          ...runtimeTelemetryProperties(
            body,
            asi,
            compatVar,
            compatFunction,
            resolvedVersion,
          ),
        },
      });
      captureServerEvent(`${config.eventPrefix}_spawn_failed`, {
        distinctId,
        path: config.path,
        properties: {
          binaryName: basename(invocation.binary),
          message: err.message,
          ...runtimeTelemetryProperties(
            body,
            asi,
            compatVar,
            compatFunction,
            resolvedVersion,
          ),
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
            resolvedVersion,
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

      const responseBody = buildResponseBody(
        config,
        invocation,
        parsed,
        stdoutText,
        stderrText,
        exitCode,
        signal,
        truncated,
      );

      // Cache only deterministic outcomes — truncated outputs are
      // incomplete, and a non-null signal / null exitCode means the runner
      // hit a resource limit (timeout, memory, instructions) rather than
      // producing a stable answer. JS runtime errors with a normal exit
      // are still cached: same source, same exception.
      if (isResponseCacheable({ truncated, signal, exitCode })) {
        responseCacheSet(cacheKey, responseBody);
      }

      finish(
        NextResponse.json(responseBody, {
          headers: {
            "X-Cache": "MISS",
            "X-RateLimit-Limit": String(rl.limit),
            "X-RateLimit-Remaining": String(rl.remaining),
            "X-RateLimit-Reset": String(Math.ceil(rl.resetAt / 1000)),
          },
        }),
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
