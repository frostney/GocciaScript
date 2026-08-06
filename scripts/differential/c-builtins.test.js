// Battery C — real semantics of the 0.11.0 built-ins, not just typeof checks.

describe("AbortController semantics", () => {
  test("fresh signal is not aborted", () => {
    const c = new AbortController();
    expect(c.signal.aborted).toBe(false);
  });

  test("abort() flips aborted and sets a default reason", () => {
    const c = new AbortController();
    c.abort();
    expect(c.signal.aborted).toBe(true);
    expect(c.signal.reason).toBeDefined();
    expect(c.signal.reason.name).toBe("AbortError");
  });

  test("abort(customReason) preserves the reason", () => {
    const c = new AbortController();
    c.abort(new Error("stop now"));
    expect(c.signal.reason.message).toBe("stop now");
  });

  test("throwIfAborted throws the reason after abort", () => {
    const c = new AbortController();
    expect(() => c.signal.throwIfAborted()).not.toThrow();
    c.abort(new Error("gone"));
    expect(() => c.signal.throwIfAborted()).toThrow("gone");
  });

  test("abort event fires listeners exactly once", () => {
    const c = new AbortController();
    let calls = 0;
    c.signal.addEventListener("abort", () => {
      calls += 1;
    });
    c.abort();
    c.abort();
    expect(calls).toBe(1);
  });

  test("AbortSignal.abort() static returns pre-aborted signal", () => {
    const s = AbortSignal.abort(new Error("static"));
    expect(s.aborted).toBe(true);
    expect(s.reason.message).toBe("static");
  });

  test("AbortSignal.timeout returns a live signal", () => {
    const s = AbortSignal.timeout(10_000);
    expect(s.aborted).toBe(false);
  });
});

describe("modern built-ins", () => {
  test("structuredClone deep-clones Map and Set", () => {
    const src = { m: new Map([["k", { v: 1 }]]), s: new Set([1, 2]) };
    const c = structuredClone(src);
    c.m.get("k").v = 99;
    expect(src.m.get("k").v).toBe(1);
    expect(c.s.has(2)).toBe(true);
  });

  test("Object.groupBy groups", () => {
    const g = Object.groupBy([1, 2, 3, 4], (n) => (n % 2 === 0 ? "even" : "odd"));
    expect(g.even).toEqual([2, 4]);
    expect(g.odd).toEqual([1, 3]);
  });

  test("Promise.withResolvers", () => {
    const { promise, resolve } = Promise.withResolvers();
    resolve(42);
    return promise.then((v) => {
      expect(v).toBe(42);
    });
  });

  test("Error cause chains", () => {
    const e = new Error("outer", { cause: new Error("inner") });
    expect(e.cause.message).toBe("inner");
  });

  test("RegExp named groups", () => {
    const m = "2026-08-05".match(/(?<y>\d{4})-(?<mo>\d{2})/);
    expect(m.groups.y).toBe("2026");
  });
});
