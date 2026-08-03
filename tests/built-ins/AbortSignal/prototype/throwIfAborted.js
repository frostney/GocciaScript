/*---
description: AbortSignal throwIfAborted behavior
features: [AbortSignal]
---*/

describe("AbortSignal.prototype.throwIfAborted", () => {
  test("does nothing for a live signal", () => {
    const signal = new AbortController().signal;
    expect(signal.throwIfAborted()).toBe(undefined);
  });

  test("throws the exact abort reason", () => {
    const reason = { kind: "stop" };
    const signal = AbortSignal.abort(reason);
    let thrown;
    try {
      signal.throwIfAborted();
    } catch (error) {
      thrown = error;
    }
    expect(thrown).toBe(reason);
  });

  test("rejects incompatible receivers", () => {
    expect(() => AbortSignal.prototype.throwIfAborted.call({})).toThrow(
      TypeError
    );
  });
});
