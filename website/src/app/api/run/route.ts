import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import path from "node:path";
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
    | "SPAWN_FAILED";
};

function transportError(
  err: TransportError,
  init?: ResponseInit & { extra?: Record<string, unknown> },
): Response {
  const { extra, ...rest } = init ?? {};
  return NextResponse.json({ error: err, ...(extra ?? {}) }, rest);
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

  let body: RunBody;
  try {
    body = (await req.json()) as RunBody;
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
    const finish = (res: Response) => {
      if (resolved) return;
      resolved = true;
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

    let stdoutBuf = "";
    let stderrBuf = "";
    let truncated = false;

    const killTimer = setTimeout(() => {
      truncated = true;
      child.kill("SIGKILL");
    }, TIMEOUT_MS + 1_500);

    child.stdout.on("data", (chunk: Buffer) => {
      const next = stdoutBuf + chunk.toString("utf8");
      if (next.length > MAX_OUTPUT_BYTES) {
        stdoutBuf = next.slice(0, MAX_OUTPUT_BYTES);
        truncated = true;
        child.kill("SIGKILL");
      } else {
        stdoutBuf = next;
      }
    });

    child.stderr.on("data", (chunk: Buffer) => {
      stderrBuf += chunk.toString("utf8");
      if (stderrBuf.length > MAX_OUTPUT_BYTES) {
        stderrBuf = stderrBuf.slice(0, MAX_OUTPUT_BYTES);
      }
    });

    child.on("error", (err) => {
      clearTimeout(killTimer);
      finish(
        transportError(
          {
            message: `spawn failed: ${err.message}`,
            code: "SPAWN_FAILED",
          },
          { status: 500, extra: { binary } },
        ),
      );
    });

    child.on("close", (exitCode, signal) => {
      clearTimeout(killTimer);
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

    child.stdin.write(code);
    child.stdin.end();
  });
}
