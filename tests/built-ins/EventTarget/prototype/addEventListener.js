/*---
description: EventTarget.prototype.addEventListener registration semantics
features: [EventTarget, Event]
---*/

describe("EventTarget.prototype.addEventListener", () => {
  test("invokes the listener for a matching type only", () => {
    const target = new EventTarget();
    let calls = 0;
    target.addEventListener("ping", () => {
      calls += 1;
    });

    target.dispatchEvent(new Event("ping"));
    target.dispatchEvent(new Event("pong"));
    expect(calls).toBe(1);
  });

  test("does not add the same type, callback and capture twice", () => {
    const target = new EventTarget();
    let calls = 0;
    const listener = () => {
      calls += 1;
    };

    target.addEventListener("ping", listener);
    target.addEventListener("ping", listener);
    target.addEventListener("ping", listener, { capture: false });
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(1);
  });

  test("treats capture as part of listener identity", () => {
    const target = new EventTarget();
    let calls = 0;
    const listener = () => {
      calls += 1;
    };

    target.addEventListener("ping", listener, true);
    target.addEventListener("ping", listener, false);
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(2);
  });

  test("removes a once listener before invoking it", () => {
    const target = new EventTarget();
    let calls = 0;
    target.addEventListener(
      "ping",
      () => {
        calls += 1;
      },
      { once: true }
    );

    target.dispatchEvent(new Event("ping"));
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(1);
  });

  test("invokes the listener in registration order", () => {
    const target = new EventTarget();
    const order = [];
    target.addEventListener("ping", () => order.push("first"));
    target.addEventListener("ping", () => order.push("second"));

    target.dispatchEvent(new Event("ping"));
    expect(order).toEqual(["first", "second"]);
  });

  test("does not invoke listeners added during dispatch", () => {
    const target = new EventTarget();
    let inner = 0;
    target.addEventListener("ping", () => {
      target.addEventListener("ping", () => {
        inner += 1;
      });
    });

    target.dispatchEvent(new Event("ping"));
    expect(inner).toBe(0);
  });

  test("invokes a non-callable object listener through handleEvent", () => {
    const target = new EventTarget();
    let received = null;
    const listener = {
      handleEvent(event) {
        received = event;
      },
    };

    target.addEventListener("ping", listener);
    const event = new Event("ping");
    target.dispatchEvent(event);
    expect(received).toBe(event);
  });

  test("uses the listener object as the handleEvent receiver", () => {
    const target = new EventTarget();
    let receiverIsListener = null;
    const listener = {
      handleEvent() {
        receiverIsListener = this === listener;
      },
    };

    target.addEventListener("ping", listener);
    target.dispatchEvent(new Event("ping"));
    expect(receiverIsListener).toBe(true);
  });

  test("invokes a listener added during dispatch on the next dispatch", () => {
    const target = new EventTarget();
    let inner = 0;
    let added = false;
    target.addEventListener("ping", () => {
      if (added) return;
      added = true;
      target.addEventListener("ping", () => {
        inner += 1;
      });
    });

    target.dispatchEvent(new Event("ping"));
    expect(inner).toBe(0);
    target.dispatchEvent(new Event("ping"));
    expect(inner).toBe(1);
  });

  test("does not invoke a listener removed and re-added during dispatch", () => {
    const target = new EventTarget();
    let calls = 0;
    let mutated = false;
    const listener = () => {
      calls += 1;
    };

    target.addEventListener("ping", () => {
      if (mutated) return;
      mutated = true;
      target.removeEventListener("ping", listener);
      target.addEventListener("ping", listener);
    });
    target.addEventListener("ping", listener);

    // The re-added listener is appended, so it is outside this dispatch.
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(0);
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(1);
  });

  test("reads the options dictionary in capture, once, passive order", () => {
    const target = new EventTarget();
    const seen = [];
    const options = {
      get capture() {
        seen.push("capture");
        return false;
      },
      get once() {
        seen.push("once");
        return false;
      },
      get passive() {
        seen.push("passive");
        return false;
      },
    };

    target.addEventListener("ping", () => {}, options);
    expect(seen).toEqual(["capture", "once", "passive"]);
  });

  test("ignores null and undefined callbacks", () => {
    const target = new EventTarget();
    target.addEventListener("ping", null);
    target.addEventListener("ping", undefined);
    expect(target.dispatchEvent(new Event("ping"))).toBe(true);
  });

  test("rejects non-object callbacks and missing arguments", () => {
    const target = new EventTarget();
    expect(() => target.addEventListener("ping", 5)).toThrow(TypeError);
    expect(() => target.addEventListener("ping", "listener")).toThrow(TypeError);
    expect(() => target.addEventListener("ping")).toThrow(TypeError);
  });

  test("rejects a symbol type", () => {
    const target = new EventTarget();
    expect(() => target.addEventListener(Symbol("ping"), () => {})).toThrow(
      TypeError
    );
  });

  test("keeps many listeners independent across removals", () => {
    const target = new EventTarget();
    const listeners = [];
    let calls = 0;

    Array.from({ length: 200 }).forEach(() => {
      const listener = () => {
        calls += 1;
      };
      listeners.push(listener);
      target.addEventListener("many", listener);
    });

    target.dispatchEvent(new Event("many"));
    expect(calls).toBe(200);

    listeners.forEach((listener, index) => {
      if (index % 2 === 0) target.removeEventListener("many", listener);
    });
    calls = 0;
    target.dispatchEvent(new Event("many"));
    expect(calls).toBe(100);
  });

  test("rejects incompatible receivers", () => {
    expect(() =>
      EventTarget.prototype.addEventListener.call({}, "ping", () => {})
    ).toThrow(TypeError);
  });
});
