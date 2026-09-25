/*---
description: AbortController signal getter behavior
features: [AbortController, AbortSignal]
---*/

describe("AbortController.prototype.signal", () => {
  test("returns the same AbortSignal object", () => {
    const controller = new AbortController();
    expect(controller.signal).toBe(controller.signal);
    expect(controller.signal instanceof AbortSignal).toBe(true);
    expect(Object.prototype.toString.call(controller.signal)).toBe(
      "[object AbortSignal]"
    );
  });

  test("starts not aborted with an undefined reason", () => {
    const signal = new AbortController().signal;
    expect(signal.aborted).toBe(false);
    expect(signal.reason).toBe(undefined);
  });

  test("rejects incompatible receivers", () => {
    const getter = Object.getOwnPropertyDescriptor(
      AbortController.prototype,
      "signal"
    ).get;
    expect(() => getter.call({})).toThrow(TypeError);
  });
});
