/*---
description: EventTarget.prototype.removeEventListener removal semantics
features: [EventTarget, Event]
---*/

describe("EventTarget.prototype.removeEventListener", () => {
  test("stops a registered listener from being invoked", () => {
    const target = new EventTarget();
    let calls = 0;
    const listener = () => {
      calls += 1;
    };

    target.addEventListener("ping", listener);
    target.dispatchEvent(new Event("ping"));
    target.removeEventListener("ping", listener);
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(1);
  });

  test("only removes the listener with a matching capture flag", () => {
    const target = new EventTarget();
    let calls = 0;
    const listener = () => {
      calls += 1;
    };

    target.addEventListener("ping", listener, true);
    target.removeEventListener("ping", listener, false);
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(1);

    target.removeEventListener("ping", listener, true);
    target.dispatchEvent(new Event("ping"));
    expect(calls).toBe(1);
  });

  test("is respected when a listener is removed during dispatch", () => {
    const target = new EventTarget();
    let removedCalls = 0;
    const removed = () => {
      removedCalls += 1;
    };

    target.addEventListener("ping", () => {
      target.removeEventListener("ping", removed);
    });
    target.addEventListener("ping", removed);

    target.dispatchEvent(new Event("ping"));
    expect(removedCalls).toBe(0);
  });

  test("ignores unknown listeners", () => {
    const target = new EventTarget();
    target.removeEventListener("ping", () => {});
    expect(target.dispatchEvent(new Event("ping"))).toBe(true);
  });

  test("ignores null and undefined callbacks", () => {
    const target = new EventTarget();
    target.removeEventListener("ping", null);
    target.removeEventListener("ping", undefined);
    expect(target.dispatchEvent(new Event("ping"))).toBe(true);
  });

  test("rejects non-object callbacks and incompatible receivers", () => {
    const target = new EventTarget();
    expect(() => target.removeEventListener("ping", 5)).toThrow(TypeError);
    expect(() => target.removeEventListener("ping")).toThrow(TypeError);
    expect(() =>
      EventTarget.prototype.removeEventListener.call({}, "ping", () => {})
    ).toThrow(TypeError);
  });
});
