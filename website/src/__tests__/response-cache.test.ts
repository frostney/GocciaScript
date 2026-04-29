import { afterEach, beforeEach, describe, expect, test } from "bun:test";
import {
  isResponseCacheable,
  isResponseCacheEnabled,
  type ResponseCacheKeyInput,
  responseCacheClear,
  responseCacheGet,
  responseCacheKey,
  responseCacheSet,
  responseCacheSize,
} from "@/lib/response-cache";

// The cache reads `Date.now()` on each access to compute expiry, so we can
// drive its internal clock by replacing the global. Each test scopes its
// own override + restore so they don't leak between tests.
let realDateNow: () => number;
beforeEach(() => {
  realDateNow = Date.now;
  // The cache is a singleton on `globalThis`; flush it so tests can't leak
  // entries into each other.
  responseCacheClear();
});
afterEach(() => {
  Date.now = realDateNow;
  responseCacheClear();
});

const baseInput = (
  overrides: Partial<ResponseCacheKeyInput> = {},
): ResponseCacheKeyInput => ({
  kind: "execute",
  code: "1 + 1;",
  mode: "interpreted",
  asi: true,
  compatVar: false,
  compatFunction: false,
  version: "0.7.0",
  ...overrides,
});

describe("responseCacheKey — stable hashing", () => {
  test("produces a stable, kind-prefixed hex hash for identical inputs", () => {
    const a = responseCacheKey(baseInput());
    const b = responseCacheKey(baseInput());
    expect(a).toBe(b);
    // 64 hex chars (sha256) plus a `kind:` prefix.
    expect(a.startsWith("execute:")).toBe(true);
    expect(a.slice("execute:".length)).toMatch(/^[0-9a-f]{64}$/);
  });

  test("differentiates keys when `kind` changes", () => {
    expect(responseCacheKey(baseInput({ kind: "execute" }))).not.toBe(
      responseCacheKey(baseInput({ kind: "test" })),
    );
  });

  test("differentiates keys when source code changes", () => {
    expect(responseCacheKey(baseInput({ code: "1 + 1;" }))).not.toBe(
      responseCacheKey(baseInput({ code: "1 + 2;" })),
    );
  });

  test("differentiates keys when the runner mode changes", () => {
    expect(responseCacheKey(baseInput({ mode: "interpreted" }))).not.toBe(
      responseCacheKey(baseInput({ mode: "bytecode" })),
    );
  });

  test("differentiates keys when the engine version changes", () => {
    // Same source against v0.7.0 and nightly may produce different output as
    // the engine evolves; cache must isolate per resolved tag.
    expect(responseCacheKey(baseInput({ version: "0.7.0" }))).not.toBe(
      responseCacheKey(baseInput({ version: "nightly" })),
    );
    expect(responseCacheKey(baseInput({ version: "0.6.1" }))).not.toBe(
      responseCacheKey(baseInput({ version: "0.7.0" })),
    );
  });

  test("differentiates keys when each boolean flag flips", () => {
    const baseline = responseCacheKey(baseInput());
    expect(baseline).not.toBe(responseCacheKey(baseInput({ asi: false })));
    expect(baseline).not.toBe(responseCacheKey(baseInput({ compatVar: true })));
    expect(baseline).not.toBe(
      responseCacheKey(baseInput({ compatFunction: true })),
    );
  });

  test("never collapses a flag flip into an adjacent code change", () => {
    // Boundary check: separators between fields prevent `code:"a", asi:true`
    // from hashing the same as `code:"atrue"`. Use minimal inputs so any
    // boundary leak would surface as a collision here.
    const a = responseCacheKey(baseInput({ code: "a", asi: true }));
    const b = responseCacheKey(baseInput({ code: "atrue", asi: false }));
    expect(a).not.toBe(b);
  });

  test("never collapses a version-string boundary into the code field", () => {
    // Without separators, `version:"ab", code:"cd"` and `version:"abc",
    // code:"d"` would both hash the bytes "abcd" at the version/code
    // boundary. The \x00 between fields prevents that collision.
    const a = responseCacheKey(baseInput({ version: "ab", code: "cd" }));
    const b = responseCacheKey(baseInput({ version: "abc", code: "d" }));
    expect(a).not.toBe(b);
  });
});

describe("responseCacheGet / responseCacheSet — TTL + LRU", () => {
  test("a fresh entry round-trips and reports HIT until TTL expires", () => {
    Date.now = () => 10_000;
    const key = "execute:demo-1";
    expect(responseCacheGet(key)).toBeNull();

    responseCacheSet(key, { ok: true, value: 2 });

    // Immediately retrievable, before TTL expires.
    expect(responseCacheGet(key)).toEqual({ ok: true, value: 2 });

    // Step the clock past the default TTL window (60s by default).
    Date.now = () => 10_000 + 120_000;
    expect(responseCacheGet(key)).toBeNull();
  });

  test("re-setting the same key refreshes the expiry", () => {
    let now = 10_000;
    Date.now = () => now;
    const key = "execute:demo-refresh";

    responseCacheSet(key, { ok: true, value: 1 });

    // Step almost to expiry, then refresh.
    now = 10_000 + 50_000;
    responseCacheSet(key, { ok: true, value: 2 });

    // Step past where the original would have expired but before the new
    // expiry (default TTL 60s, so original would die at 70s, refreshed
    // expiry sits at 110s).
    now = 10_000 + 80_000;
    expect(responseCacheGet(key)).toEqual({ ok: true, value: 2 });
  });

  test("get returns null for unknown keys without populating the cache", () => {
    expect(responseCacheGet("execute:never-set")).toBeNull();
    expect(responseCacheSize()).toBe(0);
  });

  test("get evicts an entry once it has expired", () => {
    Date.now = () => 1_000;
    const key = "execute:expire-then-evict";
    responseCacheSet(key, { ok: true });
    expect(responseCacheSize()).toBe(1);

    Date.now = () => 1_000 + 999_999;
    expect(responseCacheGet(key)).toBeNull();
    // Stale entry is dropped on miss-by-expiry — it shouldn't count toward
    // the LRU max afterwards.
    expect(responseCacheSize()).toBe(0);
  });

  test("LRU touch on get keeps recently-accessed entries alive under pressure", () => {
    // Force a tiny logical cap by setting many entries and verifying that
    // the *least-recently-touched* one is the first to be evicted. We can't
    // shrink the static cap mid-test, so instead we exercise the touch
    // semantics: setting B then C, then reading B, then setting D — B
    // survives, C becomes LRU, but with the default cap of 1024 nothing is
    // actually evicted. The behavioral check here is the order of keys()
    // after a touch.
    Date.now = () => 1_000;

    responseCacheSet("execute:a", { tag: "a" });
    responseCacheSet("execute:b", { tag: "b" });
    responseCacheSet("execute:c", { tag: "c" });

    // Touch `a`, moving it to the most-recently-used end.
    expect(responseCacheGet("execute:a")).toEqual({ tag: "a" });

    // Re-set `b`, also moving it. After this `c` is the oldest.
    responseCacheSet("execute:b", { tag: "b2" });

    // Order should now be c, a, b (oldest -> newest).
    // We can't reach into the cache to inspect order, but the contract is
    // observable through eviction: the public `responseCacheSize()` only
    // tells us count, so we settle for verifying the entries are all still
    // retrievable (no premature eviction at this scale) and that `b` shows
    // its refreshed value.
    expect(responseCacheGet("execute:a")).toEqual({ tag: "a" });
    expect(responseCacheGet("execute:b")).toEqual({ tag: "b2" });
    expect(responseCacheGet("execute:c")).toEqual({ tag: "c" });
  });

  test("returned bodies are isolated from the cached copy (no mutation poisoning)", () => {
    Date.now = () => 1_000;
    const key = "execute:isolation";
    responseCacheSet(key, { ok: true, output: ["line1"] });

    const first = responseCacheGet(key);
    expect(first).toEqual({ ok: true, output: ["line1"] });

    // Mutate the returned body — both shallow and a nested array — to prove
    // we got an independent copy. A shared reference would corrupt the next
    // hit.
    if (first) {
      (first as { ok: boolean }).ok = false;
      (first.output as string[]).push("injected");
    }

    const second = responseCacheGet(key);
    expect(second).toEqual({ ok: true, output: ["line1"] });
  });

  test("entries from different keys are isolated", () => {
    Date.now = () => 1_000;
    responseCacheSet("execute:k1", { ok: true, value: "one" });
    responseCacheSet("execute:k2", { ok: true, value: "two" });
    expect(responseCacheGet("execute:k1")).toEqual({
      ok: true,
      value: "one",
    });
    expect(responseCacheGet("execute:k2")).toEqual({
      ok: true,
      value: "two",
    });
  });

  test("isResponseCacheEnabled is true with default env (TTL 60s, max 1024)", () => {
    // Module-level config is captured at import time, so `process.env`
    // mutations after-the-fact don't change the result. The default
    // scenario (no env overrides) is what production sees, so verifying
    // it here pins the production-default behavior.
    expect(isResponseCacheEnabled()).toBe(true);
  });
});

describe("isResponseCacheable — eligibility predicate", () => {
  test("caches a clean exit with code 0", () => {
    expect(
      isResponseCacheable({ truncated: false, signal: null, exitCode: 0 }),
    ).toBe(true);
  });

  test("caches a non-zero exit (deterministic JS error)", () => {
    // A syntax error or thrown exception still yields a deterministic
    // response for the same source — same input, same error.
    expect(
      isResponseCacheable({ truncated: false, signal: null, exitCode: 1 }),
    ).toBe(true);
  });

  test("does not cache truncated output", () => {
    expect(
      isResponseCacheable({ truncated: true, signal: null, exitCode: 0 }),
    ).toBe(false);
  });

  test("does not cache when the runner was killed by a signal", () => {
    expect(
      isResponseCacheable({
        truncated: false,
        signal: "SIGKILL",
        exitCode: null,
      }),
    ).toBe(false);
  });

  test("does not cache when the runner produced no exit code", () => {
    // exitCode === null means the process didn't exit normally — typically
    // paired with a signal, but we defensively treat it as non-deterministic
    // even if signal somehow ended up null.
    expect(
      isResponseCacheable({ truncated: false, signal: null, exitCode: null }),
    ).toBe(false);
  });
});
