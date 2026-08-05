/*---
description: AbortSignal.abort behavior
features: [AbortSignal, DOMException]
---*/

describe("AbortSignal.abort", () => {
  test("returns an already-aborted signal with the supplied reason", () => {
    const reason = { kind: "stop" };
    const signal = AbortSignal.abort(reason);
    expect(signal instanceof AbortSignal).toBe(true);
    expect(signal.aborted).toBe(true);
    expect(signal.reason).toBe(reason);
  });

  test("uses an AbortError DOMException by default", () => {
    const signal = AbortSignal.abort();
    expect(signal.reason instanceof DOMException).toBe(true);
    expect(signal.reason.name).toBe("AbortError");
    expect(signal.reason.code).toBe(20);
  });
});
