/*---
description: AbortController abort behavior
features: [AbortController, AbortSignal, DOMException]
---*/

describe("AbortController.prototype.abort", () => {
  test("aborts once with the supplied reason", () => {
    const controller = new AbortController();
    const reason = { kind: "cancelled" };
    controller.abort(reason);
    controller.abort("replacement");

    expect(controller.signal.aborted).toBe(true);
    expect(controller.signal.reason).toBe(reason);
  });

  test("uses an AbortError DOMException by default", () => {
    const controller = new AbortController();
    controller.abort();

    expect(controller.signal.reason instanceof DOMException).toBe(true);
    expect(controller.signal.reason.name).toBe("AbortError");
    expect(controller.signal.reason.code).toBe(20);
  });

  test("treats an explicit undefined reason as the default", () => {
    const controller = new AbortController();
    controller.abort(undefined);
    expect(controller.signal.reason.name).toBe("AbortError");
  });

  test("fires the abort event synchronously within abort()", () => {
    const controller = new AbortController();
    const order = [];
    controller.signal.addEventListener("abort", () => order.push("listener"));

    order.push("before");
    controller.abort();
    order.push("after");
    expect(order).toEqual(["before", "listener", "after"]);
  });

  test("aborts before the listener observes the signal state", () => {
    const controller = new AbortController();
    let abortedDuringDispatch = null;
    controller.signal.addEventListener("abort", () => {
      abortedDuringDispatch = controller.signal.aborted;
    });

    controller.abort();
    expect(abortedDuringDispatch).toBe(true);
  });

  test("rejects incompatible receivers", () => {
    expect(() => AbortController.prototype.abort.call({})).toThrow(TypeError);
  });
});
