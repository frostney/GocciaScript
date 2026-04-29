// In-memory TTL + LRU cache for runner responses, so a client mashing the
// "Run" button on the playground doesn't spawn a fresh GocciaScriptLoader /
// GocciaTestRunner subprocess each time when nothing about the input has
// changed. Same input -> same cached response within the TTL window.
//
// Adequate for single-instance Vercel functions and dev servers; cache hits
// won't be shared across function instances. For cross-instance dedup, swap
// to Vercel KV / Upstash Redis behind the same get/set surface.
//
// This is paired with `rate-limit.ts`: the rate limiter bounds total request
// volume per IP (including cache hits, which still consume bandwidth and a
// connection slot), while the cache bounds the *server* CPU cost when many
// of those requests carry the same payload.

import { createHash } from "node:crypto";

type CacheEntry = {
  body: Record<string, unknown>;
  expiresAt: number;
};

const CACHE_CONFIG: { ttlMs: number; maxEntries: number } = {
  ttlMs: Number(process.env.RESPONSE_CACHE_TTL_MS ?? 60_000),
  maxEntries: Number(process.env.RESPONSE_CACHE_MAX_ENTRIES ?? 1024),
};

// Cross-module global on the runtime instance, so HMR/edit reloads don't
// drop the cache. Same trick `rate-limit.ts` uses for its bucket map; keep
// the typed accessor so the rest of the file stays strict.
type CacheGlobal = typeof globalThis & {
  __GOCCIA_RESPONSE_CACHE__?: Map<string, CacheEntry>;
};
function getCacheStore(): Map<string, CacheEntry> {
  const g = globalThis as CacheGlobal;
  if (!g.__GOCCIA_RESPONSE_CACHE__) {
    g.__GOCCIA_RESPONSE_CACHE__ = new Map();
  }
  return g.__GOCCIA_RESPONSE_CACHE__;
}
const cache = getCacheStore();

export function isResponseCacheEnabled(): boolean {
  return (
    Number.isFinite(CACHE_CONFIG.ttlMs) &&
    CACHE_CONFIG.ttlMs > 0 &&
    Number.isFinite(CACHE_CONFIG.maxEntries) &&
    CACHE_CONFIG.maxEntries > 0
  );
}

/** Inputs that materially change the runner response. Anything that does
 *  NOT affect runner output (client IP, request headers, distinct id, etc.)
 *  must stay out of this set, or two semantically-equivalent requests would
 *  miss the cache. */
export type ResponseCacheKeyInput = {
  kind: "execute" | "test";
  code: string;
  mode: "interpreted" | "bytecode";
  asi: boolean;
  compatVar: boolean;
  compatFunction: boolean;
};

export function responseCacheKey(input: ResponseCacheKeyInput): string {
  // SHA-256 over a stable serialization. The code field can be up to 8 KiB,
  // so hashing keeps Map keys at a fixed 64 hex chars and makes lookups O(1)
  // on the key length, not on the source-code length. Field separators
  // prevent boundary collisions (e.g. `code:"a", mode:"b"` vs `code:"ab"`).
  const hash = createHash("sha256");
  hash.update(input.kind);
  hash.update("\x00");
  hash.update(input.mode);
  hash.update("\x00");
  hash.update(input.asi ? "1" : "0");
  hash.update(input.compatVar ? "1" : "0");
  hash.update(input.compatFunction ? "1" : "0");
  hash.update("\x00");
  hash.update(input.code);
  // Prefix with kind so debuggers / log scrapers can tell execute and test
  // entries apart at a glance even though the hash already separates them.
  return `${input.kind}:${hash.digest("hex")}`;
}

export function responseCacheGet(key: string): Record<string, unknown> | null {
  if (!isResponseCacheEnabled()) return null;
  const entry = cache.get(key);
  if (!entry) return null;
  if (Date.now() >= entry.expiresAt) {
    cache.delete(key);
    return null;
  }
  // LRU touch: re-inserting moves the key to the most-recently-used end of
  // the Map's insertion order, so eviction can pull from the front.
  cache.delete(key);
  cache.set(key, entry);
  // Deep-clone before returning so the caller can't mutate the cached body
  // (which would corrupt every subsequent hit). The body is JSON-shaped, so
  // `structuredClone` is fast — well under the cost of the subprocess spawn
  // we're avoiding.
  return structuredClone(entry.body);
}

export function responseCacheSet(
  key: string,
  body: Record<string, unknown>,
): void {
  if (!isResponseCacheEnabled()) return;
  // Refresh insertion order on overwrite so a re-cached entry doesn't keep
  // its old position and get evicted prematurely.
  cache.delete(key);
  cache.set(key, { body, expiresAt: Date.now() + CACHE_CONFIG.ttlMs });

  // Evict the oldest (front of insertion order) until back under the cap.
  // Maps preserve insertion order, so `keys().next()` gives the LRU key.
  while (cache.size > CACHE_CONFIG.maxEntries) {
    const oldest = cache.keys().next();
    if (oldest.done) break;
    cache.delete(oldest.value);
  }
}

/** Whether a finished runner response is safe to cache. Truncated outputs
 *  are incomplete by definition; a non-null signal or a null exit code
 *  means the process was killed (timeout / memory / instruction limit /
 *  segfault), so the response describes a resource state, not a
 *  deterministic execution. JS runtime errors with a normal exit ARE
 *  cached — same source, same exception. */
export function isResponseCacheable(opts: {
  truncated: boolean;
  signal: NodeJS.Signals | null;
  exitCode: number | null;
}): boolean {
  if (opts.truncated) return false;
  if (opts.signal !== null) return false;
  if (opts.exitCode === null) return false;
  return true;
}

/** Test helper. Not called from any HTTP route. */
export function responseCacheClear(): void {
  cache.clear();
}

/** Test helper. Not called from any HTTP route. */
export function responseCacheSize(): number {
  return cache.size;
}
