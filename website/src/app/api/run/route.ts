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
    return NextResponse.json(
      { error: "rate limit exceeded", retryAfter },
      {
        status: 429,
        headers: {
          "Retry-After": String(retryAfter),
          "X-RateLimit-Limit": String(rl.limit),
          "X-RateLimit-Remaining": "0",
          "X-RateLimit-Reset": String(Math.ceil(rl.resetAt / 1000)),
        },
      },
    );
  }

  let body: RunBody;
  try {
    body = (await req.json()) as RunBody;
  } catch {
    return NextResponse.json({ error: "invalid JSON body" }, { status: 400 });
  }

  const code = typeof body.code === "string" ? body.code : "";
  if (!code) {
    return NextResponse.json({ error: "code is required" }, { status: 400 });
  }
  if (code.length > MAX_CODE_BYTES) {
    return NextResponse.json(
      { error: `code exceeds ${MAX_CODE_BYTES} bytes` },
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

    const child = spawn(binary, args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: { ...process.env, NO_COLOR: "1" },
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
        NextResponse.json(
          {
            error: `spawn failed: ${err.message}`,
            binary,
          },
          { status: 500 },
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
