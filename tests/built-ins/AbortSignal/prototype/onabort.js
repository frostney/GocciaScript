/*---
description: AbortSignal.prototype.onabort event handler attribute
features: [AbortSignal, AbortController, Event]
---*/

describe("AbortSignal.prototype.onabort", () => {
  test("defaults to null", () => {
    const controller = new AbortController();
    expect(controller.signal.onabort).toBe(null);
  });

  test("returns the assigned handler", () => {
    const controller = new AbortController();
    const handler = () => {};
    controller.signal.onabort = handler;
    expect(controller.signal.onabort).toBe(handler);
  });

  test("fires with the abort event", () => {
    const controller = new AbortController();
    let received = null;
    controller.signal.onabort = (event) => {
      received = event;
    };

    controller.abort();
    expect(received.type).toBe("abort");
    expect(received.target).toBe(controller.signal);
  });

  test("fires at most once", () => {
    const controller = new AbortController();
    let calls = 0;
    controller.signal.onabort = () => {
      calls += 1;
    };

    controller.abort();
    controller.abort();
    expect(calls).toBe(1);
  });

  test("clears the handler when set to null", () => {
    const controller = new AbortController();
    let calls = 0;
    controller.signal.onabort = () => {
      calls += 1;
    };
    controller.signal.onabort = null;
    expect(controller.signal.onabort).toBe(null);

    controller.abort();
    expect(calls).toBe(0);
  });

  test("replaces a previously assigned handler", () => {
    const controller = new AbortController();
    const order = [];
    controller.signal.onabort = () => order.push("first");
    controller.signal.onabort = () => order.push("second");

    controller.abort();
    expect(order).toEqual(["second"]);
  });

  test("ignores non-callable assignments", () => {
    const controller = new AbortController();
    controller.signal.onabort = 42;
    expect(controller.signal.onabort).toBe(null);
  });

  test("runs alongside addEventListener listeners in registration order", () => {
    const controller = new AbortController();
    const order = [];
    controller.signal.onabort = () => order.push("handler");
    controller.signal.addEventListener("abort", () => order.push("listener"));

    controller.abort();
    expect(order).toEqual(["handler", "listener"]);
  });

  test("rejects incompatible receivers", () => {
    const descriptor = Object.getOwnPropertyDescriptor(
      AbortSignal.prototype,
      "onabort"
    );
    expect(() => descriptor.get.call({})).toThrow(TypeError);
    expect(() => descriptor.set.call({}, () => {})).toThrow(TypeError);
  });
});
