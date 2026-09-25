/*---
description: AbortSignal.timeout construction and validation
features: [AbortSignal]
---*/

describe("AbortSignal.timeout", () => {
  test("returns a live AbortSignal for a future deadline", () => {
    const signal = AbortSignal.timeout(60000);
    expect(signal instanceof AbortSignal).toBe(true);
    expect(signal.aborted).toBe(false);
    expect(signal.reason).toBe(undefined);
  });

  test("requires a timeout argument", () => {
    expect(() => AbortSignal.timeout()).toThrow(TypeError);
  });

  // GocciaScript has no timer task queue (ADR 0031, ADR 0104): an expired
  // timeout signal aborts at the moment the host observes it, and its abort
  // event is delivered at that same observation point.
  test("fires the abort event when the expired timeout is first observed", () => {
    const signal = AbortSignal.timeout(0);
    let calls = 0;
    let reasonDuringDispatch = null;
    signal.addEventListener("abort", (event) => {
      calls += 1;
      reasonDuringDispatch = event.target.reason.name;
    });

    expect(calls).toBe(0);
    expect(signal.aborted).toBe(true);
    expect(calls).toBe(1);
    expect(reasonDuringDispatch).toBe("TimeoutError");

    expect(signal.aborted).toBe(true);
    expect(signal.reason.name).toBe("TimeoutError");
    expect(calls).toBe(1);
  });

  test("never fires for a listener added after the timeout was observed", () => {
    const signal = AbortSignal.timeout(0);
    expect(signal.aborted).toBe(true);

    let fired = false;
    signal.addEventListener("abort", () => {
      fired = true;
    });
    expect(fired).toBe(false);
  });

  test("rejects invalid unsigned long long values", () => {
    expect(() => AbortSignal.timeout(NaN)).toThrow(TypeError);
    expect(() => AbortSignal.timeout(-1)).toThrow(TypeError);
    expect(() => AbortSignal.timeout(Infinity)).toThrow(TypeError);
    expect(() => AbortSignal.timeout(1e20)).toThrow(TypeError);
  });
});
