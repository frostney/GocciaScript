import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import path, { basename } from "node:path";
import { NextResponse } from "next/server";
import { clientIp, rateLimit } from "@/lib/rate-limit";

export const runtime = "nodejs";
export const dynamic = "force-dynamic";

// Resolution order:
//   1. GOCCIA_BINARY env var (use in deploys / overrides)
//   2. Bundled binary in vendor/ (Vercel deploy bundles it via outputFileTracingIncludes)
//   3. Local dev: the freshly built binary at ../build/GocciaScriptLoader
function resolveBinaryPath(): string {
  if (process.env.GOCCIA_BINARY) return process.env.GOCCIA_BINARY;
  const cwd = process.cwd();
  const candidates = [
    path.join(cwd, "vendor", "GocciaScriptLoader"),
    path.join(cwd, "..", "build", "GocciaScriptLoader"),
  ];
  for (const c of candidates) {
    if (existsSync(c)) return c;
  }
  return candidates[candidates.length - 1];
}

const TIMEOUT_MS = 5_000;
const MAX_OUTPUT_BYTES = 256 * 1024;
const MAX_CODE_BYTES = 64 * 1024;
// Cap the entire JSON envelope (`{"code":"...","mode":"…","asi":…}`) so we
// don't buffer arbitrarily large bodies before the per-field size check
// fires. Allow ~1 KB of envelope/key/value overhead on top of the code limit.
const MAX_BODY_BYTES = MAX_CODE_BYTES + 1_024;
const MAX_MEMORY_BYTES = 32 * 1024 * 1024;
const MAX_INSTRUCTIONS = 50_000_000;
const STACK_SIZE = 2_000;
const ALLOWED_HOSTS = ["icanhazdadjoke.com"];

type RunBody = {
  code: string;
  mode?: "interpreted" | "bytecode";
  asi?: boolean;
};

/** All early-error responses (rate limit, bad body, oversize) follow this
 *  same shape, so the client only ever has to read `error.message` /
 *  `error.code`. The runner's own structured `error` (different schema —
 *  `name`, `line`, `column`, `stack`, …) lives on the success-path body
 *  alongside `output` / `value` / `timing` and is not affected. */
type TransportError = {
  message: string;
  code:
    | "RATE_LIMIT"
    | "INVALID_JSON"
    | "MISSING_CODE"
    | "CODE_TOO_LARGE"
    | "SPAWN_FAILED"
    | "ABORTED";
};

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
 *  buffered. Returns `null` on overflow. */
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
      // Drain the cancellation but stop accumulating.
      try {
        await reader.cancel();
      } catch {}
      return null;
    }
    chunks.push(value);
  }
  return Buffer.concat(chunks.map((c) => Buffer.from(c))).toString("utf8");
}

type RunnerJson = {
  ok: boolean;
  value: unknown;
  output: string;
  error: {
    type?: string;
    name?: string;
    message: string;
    line?: number | null;
    column?: number | null;
    fileName?: string | null;
    stack?: string;
  } | null;
  timing: {
    lex_ms: number;
    parse_ms: number;
    compile_ms: number;
    exec_ms: number;
    total_ms: number;
  };
};

export async function POST(req: Request) {
  const ip = clientIp(req.headers);
  const rl = rateLimit(`run:${ip}`);
  if (!rl.ok) {
    const retryAfter = Math.max(1, Math.ceil((rl.resetAt - Date.now()) / 1000));
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

  // Reject oversized payloads BEFORE parsing — `req.json()` would buffer
  // the entire body first, which is a DoS vector for arbitrarily large
  // requests. When `Content-Length` is present we short-circuit on the
  // header; otherwise we stream-read with the same cap.
  const cl = req.headers.get("content-length");
  if (cl !== null) {
    const declared = Number(cl);
    if (Number.isFinite(declared) && declared > MAX_BODY_BYTES) {
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
    return transportError(
      {
        message: `request body exceeds ${MAX_BODY_BYTES} bytes`,
        code: "CODE_TOO_LARGE",
      },
      { status: 413 },
    );
  }

  let body: RunBody;
  try {
    body = JSON.parse(rawBody) as RunBody;
  } catch {
    return transportError(
      { message: "invalid JSON body", code: "INVALID_JSON" },
      { status: 400 },
    );
  }

  const code = typeof body.code === "string" ? body.code : "";
  if (!code) {
    return transportError(
      { message: "code is required", code: "MISSING_CODE" },
      { status: 400 },
    );
  }
  // UTF-16 code-unit count (`code.length`) under-counts for non-BMP source —
  // measure the actual bytes that will hit the runner's stdin instead.
  const codeBytes = Buffer.byteLength(code, "utf8");
  if (codeBytes > MAX_CODE_BYTES) {
    return transportError(
      {
        message: `code exceeds ${MAX_CODE_BYTES} bytes`,
        code: "CODE_TOO_LARGE",
      },
      { status: 413 },
    );
  }

  // Default to ASI on — matches the project's standard test/run posture.
  // Only disable when the client explicitly opts out via `asi: false`.
  const asi = body.asi !== false;
  const args = [
    "--output=json",
    `--timeout=${TIMEOUT_MS}`,
    `--max-memory=${MAX_MEMORY_BYTES}`,
    `--max-instructions=${MAX_INSTRUCTIONS}`,
    `--stack-size=${STACK_SIZE}`,
    ...ALLOWED_HOSTS.flatMap((host) => ["--allowed-host", host]),
  ];
  if (asi) args.unshift("--asi");
  if (body.mode === "bytecode") args.push("--mode=bytecode");

  const binary = resolveBinaryPath();

  return await new Promise<Response>((resolve) => {
    let resolved = false;
    let abortHandler: (() => void) | null = null;
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
    // (DB URLs, API tokens, deploy-platform vars, …) into the sandboxed
    // loader. Only the variables the binary itself needs are passed through.
    // Typed as `NodeJS.ProcessEnv` (string | undefined values) so spawn's
    // overload signatures resolve correctly.
    const ALLOWED_ENV = [
      "PATH",
      "HOME",
      "TMPDIR",
      "LANG",
      "LC_ALL",
      "TZ",
      // Next.js augments `ProcessEnv` to require `NODE_ENV`; passing it
      // through is also harmless for the loader.
      "NODE_ENV",
    ];
    const childEnv: Record<string, string | undefined> = { NO_COLOR: "1" };
    for (const k of ALLOWED_ENV) {
      const v = process.env[k];
      if (typeof v === "string") childEnv[k] = v;
    }

    const child = spawn(binary, args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: childEnv as NodeJS.ProcessEnv,
    });

    // If the client disconnects mid-execution, kill the child immediately
    // instead of letting it run until the watchdog timer fires. The
    // listener is removed in `finish()` so we don't leak it across
    // requests when the child finishes naturally first.
    abortHandler = () => {
      child.kill("SIGKILL");
      finish(
        transportError(
          { message: "client disconnected", code: "ABORTED" },
          // 499 = nginx-style "Client Closed Request"; not a registered
          // status but widely understood and not consumed by the client
          // (which already closed).
          { status: 499 },
        ),
      );
    };
    if (req.signal.aborted) {
      // Already aborted before we even spawned — fail fast.
      abortHandler();
      return;
    }
    req.signal.addEventListener("abort", abortHandler, { once: true });

    // Accumulate raw bytes per stream — counting `string.length` would
    // count UTF-16 code units, not bytes, so multibyte UTF-8 output
    // (emoji, non-Latin scripts) could blow past `MAX_OUTPUT_BYTES`
    // before we noticed. We keep the chunks as `Buffer`s, slice on
    // byte boundaries when we hit the limit, and decode once at the end.
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

    child.on("error", (err) => {
      clearTimeout(killTimer);
      // Don't leak the absolute server path back to the client — only
      // the basename is useful for "is the binary missing?" diagnostics.
      // The full path stays in server-side logs for the operator.
      console.error(
        `[/api/run] spawn failed for binary: ${binary} — ${err.message}`,
      );
      finish(
        transportError(
          {
            message: `spawn failed: ${err.message}`,
            code: "SPAWN_FAILED",
          },
          { status: 500, extra: { binary: basename(binary) } },
        ),
      );
    });

    child.on("close", (exitCode, signal) => {
      clearTimeout(killTimer);
      const stdoutBuf = Buffer.concat(stdoutChunks).toString("utf8");
      const stderrBuf = Buffer.concat(stderrChunks).toString("utf8");
      let parsed: RunnerJson | null = null;
      try {
        parsed = JSON.parse(stdoutBuf) as RunnerJson;
      } catch {
        // fall through to raw payload
      }
      finish(
        NextResponse.json(
          {
            ok: parsed?.ok ?? false,
            value: parsed?.value ?? null,
            output: parsed?.output ?? "",
            error: parsed?.error ?? null,
            timing: parsed?.timing ?? null,
            exitCode,
            signal,
            truncated,
            stderr: stderrBuf || undefined,
            rawStdout: parsed ? undefined : stdoutBuf || undefined,
          },
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

    // The child can close stdin before we finish writing (early exit on
    // parse error, OOM, killed by our timeout, etc.) — that surfaces as
    // an EPIPE error on this stream. Without a listener Node treats it
    // as unhandled and crashes the request worker. We silently drop the
    // write: the runner's own `error` / `close` event will produce the
    // response payload; partial stdin is part of the same failure mode.
    child.stdin.on("error", (err: NodeJS.ErrnoException) => {
      if (err && err.code !== "EPIPE") {
        // Anything other than the expected pipe-closed case still
        // surfaces via stderr / the close handler; log for the operator.
        console.error("[/api/run] stdin error:", err);
      }
    });
    child.stdin.write(code);
    child.stdin.end();
  });
}
