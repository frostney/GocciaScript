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

  test("rejects values outside the unsigned long long range", () => {
    expect(() => AbortSignal.timeout(-1)).toThrow(TypeError);
    expect(() => AbortSignal.timeout(Infinity)).toThrow(TypeError);
    expect(() => AbortSignal.timeout(1e20)).toThrow(TypeError);
  });
});
