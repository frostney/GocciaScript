/*---
description: Event.prototype.preventDefault cancellation semantics
features: [Event, EventTarget]
---*/

describe("Event.prototype.preventDefault", () => {
  test("sets defaultPrevented on a cancelable event", () => {
    const event = new Event("ping", { cancelable: true });
    event.preventDefault();
    expect(event.defaultPrevented).toBe(true);
  });

  test("is a no-op on a non-cancelable event", () => {
    const event = new Event("ping");
    event.preventDefault();
    expect(event.defaultPrevented).toBe(false);
  });

  test("makes dispatchEvent report cancellation", () => {
    const target = new EventTarget();
    target.addEventListener("ping", (event) => event.preventDefault());
    expect(target.dispatchEvent(new Event("ping", { cancelable: true }))).toBe(
      false
    );
  });

  test("rejects incompatible receivers", () => {
    expect(() => Event.prototype.preventDefault.call({})).toThrow(TypeError);
  });
});
