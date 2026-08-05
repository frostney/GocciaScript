/*---
description: EventTarget.prototype.dispatchEvent dispatch semantics
features: [EventTarget, Event, DOMException]
---*/

describe("EventTarget.prototype.dispatchEvent", () => {
  test("delivers the same event object to the listener", () => {
    const target = new EventTarget();
    let received = null;
    target.addEventListener("ping", (event) => {
      received = event;
    });

    const event = new Event("ping");
    expect(target.dispatchEvent(event)).toBe(true);
    expect(received).toBe(event);
    expect(received.type).toBe("ping");
  });

  test("sets target for the dispatch and clears currentTarget after it", () => {
    const target = new EventTarget();
    let currentDuringDispatch = null;
    target.addEventListener("ping", (event) => {
      currentDuringDispatch = event.currentTarget;
    });

    const event = new Event("ping");
    expect(event.target).toBe(null);
    target.dispatchEvent(event);
    expect(currentDuringDispatch).toBe(target);
    expect(event.target).toBe(target);
    expect(event.currentTarget).toBe(null);
  });

  test("returns false when a cancelable event is canceled", () => {
    const target = new EventTarget();
    target.addEventListener("ping", (event) => event.preventDefault());

    expect(target.dispatchEvent(new Event("ping", { cancelable: true }))).toBe(
      false
    );
    expect(target.dispatchEvent(new Event("ping"))).toBe(true);
  });

  test("returns true when no listener is registered", () => {
    const target = new EventTarget();
    expect(target.dispatchEvent(new Event("ping"))).toBe(true);
  });

  test("throws InvalidStateError while the event is being dispatched", () => {
    const target = new EventTarget();
    let thrown = null;
    target.addEventListener("ping", (event) => {
      try {
        target.dispatchEvent(event);
      } catch (error) {
        thrown = error;
      }
    });

    target.dispatchEvent(new Event("ping"));
    expect(thrown instanceof DOMException).toBe(true);
    expect(thrown.name).toBe("InvalidStateError");
    expect(thrown.code).toBe(11);
  });

  test("allows the same event object to be dispatched again afterwards", () => {
    const target = new EventTarget();
    let calls = 0;
    target.addEventListener("ping", () => {
      calls += 1;
    });

    const event = new Event("ping");
    target.dispatchEvent(event);
    target.dispatchEvent(event);
    expect(calls).toBe(2);
  });

  test("supports nested dispatch of a different event type", () => {
    const target = new EventTarget();
    const order = [];
    target.addEventListener("outer", () => {
      order.push("outer");
      target.dispatchEvent(new Event("inner"));
      order.push("outer-end");
    });
    target.addEventListener("inner", () => order.push("inner"));

    target.dispatchEvent(new Event("outer"));
    expect(order).toEqual(["outer", "inner", "outer-end"]);
  });

  // GocciaScript deviation (ADR 0104): WHATWG reports a listener exception to a
  // global error handler, which this runtime does not have, so it propagates.
  test("propagates an exception thrown by a listener", () => {
    const target = new EventTarget();
    target.addEventListener("boom", () => {
      throw new RangeError("listener failed");
    });

    expect(() => target.dispatchEvent(new Event("boom"))).toThrow(RangeError);
  });

  test("unwinds dispatch state when a listener throws", () => {
    const target = new EventTarget();
    target.addEventListener("boom", () => {
      throw new RangeError("listener failed");
    });

    const event = new Event("boom");
    expect(() => target.dispatchEvent(event)).toThrow(RangeError);
    expect(event.currentTarget).toBe(null);
    // The dispatch flag was cleared, so the event is still reusable.
    expect(() => target.dispatchEvent(event)).toThrow(RangeError);
  });

  test("rejects arguments that are not events", () => {
    const target = new EventTarget();
    expect(() => target.dispatchEvent()).toThrow(TypeError);
    expect(() => target.dispatchEvent({})).toThrow(TypeError);
    expect(() => target.dispatchEvent("ping")).toThrow(TypeError);
  });

  test("rejects incompatible receivers", () => {
    expect(() =>
      EventTarget.prototype.dispatchEvent.call({}, new Event("ping"))
    ).toThrow(TypeError);
  });
});
