// Per-IP fixed-window rate limiter, in-memory.
//
// Adequate for single-instance Vercel functions and dev servers; for proper
// distributed enforcement across instances, swap to Vercel KV / Upstash
// Redis. The token-bucket interface stays the same.

type Bucket = { count: number; resetAt: number };

const RATE: { max: number; windowMs: number } = {
  max: Number(process.env.RATE_LIMIT_MAX ?? 30),
  windowMs: Number(process.env.RATE_LIMIT_WINDOW_MS ?? 60_000),
};

// Cross-module global on the runtime instance, so HMR/edit reloads don't drop
// the rate-limit map. Wrap in a typed accessor to keep the rest of the file
// strict-typed.
type RateGlobal = typeof globalThis & {
  __GOCCIA_RL_BUCKETS__?: Map<string, Bucket>;
};
function getBucketStore(): Map<string, Bucket> {
  const g = globalThis as RateGlobal;
  if (!g.__GOCCIA_RL_BUCKETS__) {
    g.__GOCCIA_RL_BUCKETS__ = new Map();
  }
  return g.__GOCCIA_RL_BUCKETS__;
}
const buckets = getBucketStore();

export type RateLimitResult = {
  ok: boolean;
  remaining: number;
  resetAt: number;
  limit: number;
};

export function rateLimit(key: string): RateLimitResult {
  const now = Date.now();
  let b = buckets.get(key);
  if (!b || now >= b.resetAt) {
    b = { count: 0, resetAt: now + RATE.windowMs };
    buckets.set(key, b);
  }
  b.count += 1;
  // Opportunistic GC: drop a few stale entries every so often so the map
  // doesn't grow unbounded under traffic.
  if (buckets.size > 4096) {
    for (const [k, v] of buckets) {
      if (v.resetAt < now) buckets.delete(k);
    }
  }
  return {
    ok: b.count <= RATE.max,
    remaining: Math.max(0, RATE.max - b.count),
    resetAt: b.resetAt,
    limit: RATE.max,
  };
}

export function clientIp(headers: Headers): string {
  const fwd = headers.get("x-forwarded-for");
  if (fwd) return fwd.split(",")[0]?.trim() || "unknown";
  return (
    headers.get("x-real-ip") ??
    headers.get("cf-connecting-ip") ??
    headers.get("x-vercel-forwarded-for") ??
    "unknown"
  );
}
