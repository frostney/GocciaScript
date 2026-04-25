import { afterEach, beforeEach, describe, expect, test } from "bun:test";
import { clientIp, rateLimit } from "@/lib/rate-limit";

// The rate limiter reads `Date.now()` on each call, so we can drive its
// internal clock by replacing the global. Each test scopes its own
// override + restore so they don't leak between tests.
let realDateNow: () => number;
beforeEach(() => {
  realDateNow = Date.now;
});
afterEach(() => {
  Date.now = realDateNow;
});

/** Generate a unique key per test so the singleton bucket map can't
 *  leak state between tests in the same file. */
let keyCounter = 0;
const uniqueKey = (prefix: string) => `${prefix}:${++keyCounter}`;

describe("rateLimit — token-bucket window", () => {
  test("first request fills the first slot of a fresh window", () => {
    Date.now = () => 1_000;
    const key = uniqueKey("first");
    const r = rateLimit(key);
    expect(r.ok).toBe(true);
    expect(r.remaining).toBe(r.limit - 1);
    expect(r.resetAt).toBeGreaterThan(1_000);
  });

  test("requests within the window count down `remaining` monotonically", () => {
    Date.now = () => 2_000;
    const key = uniqueKey("countdown");
    const a = rateLimit(key);
    const b = rateLimit(key);
    const c = rateLimit(key);
    expect(b.remaining).toBe(a.remaining - 1);
    expect(c.remaining).toBe(b.remaining - 1);
    expect(a.resetAt).toBe(b.resetAt);
    expect(b.resetAt).toBe(c.resetAt);
  });

  test("once the bucket is exhausted, `ok` is false and `remaining` clamps at 0", () => {
    Date.now = () => 3_000;
    const key = uniqueKey("exhaust");
    let last = rateLimit(key);
    while (last.ok) last = rateLimit(key);
    expect(last.ok).toBe(false);
    expect(last.remaining).toBe(0);
    // Further calls in the same window stay denied.
    const next = rateLimit(key);
    expect(next.ok).toBe(false);
  });

  test("crossing `resetAt` opens a fresh window with full capacity", () => {
    let now = 10_000;
    Date.now = () => now;
    const key = uniqueKey("expiry");

    const first = rateLimit(key);
    expect(first.ok).toBe(true);
    const limit = first.limit;
    const reset = first.resetAt;

    // Burn the rest of this window's budget.
    for (let i = 1; i < limit; i++) rateLimit(key);
    expect(rateLimit(key).ok).toBe(false);

    // Step the clock past the window boundary — the bucket resets.
    now = reset + 1;
    const after = rateLimit(key);
    expect(after.ok).toBe(true);
    expect(after.remaining).toBe(limit - 1);
    expect(after.resetAt).toBeGreaterThan(reset);
  });

  test("different keys get independent buckets", () => {
    Date.now = () => 20_000;
    const a = uniqueKey("isoA");
    const b = uniqueKey("isoB");

    let last = rateLimit(a);
    while (last.ok) last = rateLimit(a);
    expect(last.ok).toBe(false);

    // `b`'s first hit is unaffected by `a` exhausting itself.
    const fresh = rateLimit(b);
    expect(fresh.ok).toBe(true);
    expect(fresh.remaining).toBe(fresh.limit - 1);
  });
});

describe("clientIp — header preference order", () => {
  function headersOf(entries: Record<string, string>): Headers {
    const h = new Headers();
    for (const [k, v] of Object.entries(entries)) h.set(k, v);
    return h;
  }

  test("prefers cf-connecting-ip when present", () => {
    const h = headersOf({
      "cf-connecting-ip": "203.0.113.7",
      "x-real-ip": "10.0.0.1",
      "x-vercel-forwarded-for": "10.0.0.2",
      "x-forwarded-for": "10.0.0.3",
    });
    expect(clientIp(h)).toBe("203.0.113.7");
  });

  test("falls back to x-real-ip", () => {
    expect(
      clientIp(
        headersOf({ "x-real-ip": "203.0.113.8", "x-forwarded-for": "evil" }),
      ),
    ).toBe("203.0.113.8");
  });

  test("falls back to x-vercel-forwarded-for after platform-specific headers", () => {
    expect(
      clientIp(
        headersOf({
          "x-vercel-forwarded-for": "203.0.113.9",
          "x-forwarded-for": "evil",
        }),
      ),
    ).toBe("203.0.113.9");
  });

  test("uses x-forwarded-for only as a last resort, picking the right-most segment", () => {
    // Right-most is the address the closest trusted proxy appended;
    // left-most is whatever the client (potentially) said.
    expect(
      clientIp(
        headersOf({
          "x-forwarded-for": "client.spoof, 10.0.0.1, 203.0.113.10",
        }),
      ),
    ).toBe("203.0.113.10");
  });

  test("collapses surrounding whitespace and skips empty segments in x-forwarded-for", () => {
    expect(
      clientIp(headersOf({ "x-forwarded-for": "  ,  203.0.113.11  ,  " })),
    ).toBe("203.0.113.11");
  });

  test("returns 'unknown' when no useful header is present", () => {
    expect(clientIp(new Headers())).toBe("unknown");
  });
});
