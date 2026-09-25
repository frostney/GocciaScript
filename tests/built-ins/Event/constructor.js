/*---
description: Event constructor and instance state
features: [Event, EventTarget]
---*/

describe("Event constructor", () => {
  test("is exposed as a runtime global", () => {
    expect(typeof Event).toBe("function");
    expect(Goccia.runtimeGlobals.includes("Event")).toBe(true);
  });

  test("creates an Event with the supplied type", () => {
    const event = new Event("ping");
    expect(event instanceof Event).toBe(true);
    expect(event.type).toBe("ping");
    expect(Object.prototype.toString.call(event)).toBe("[object Event]");
  });

  test("defaults bubbles, cancelable and defaultPrevented to false", () => {
    const event = new Event("ping");
    expect(event.bubbles).toBe(false);
    expect(event.cancelable).toBe(false);
    expect(event.defaultPrevented).toBe(false);
  });

  test("reads bubbles and cancelable from the init dictionary", () => {
    const event = new Event("ping", { bubbles: true, cancelable: true });
    expect(event.bubbles).toBe(true);
    expect(event.cancelable).toBe(true);
  });

  test("starts with a null target and currentTarget", () => {
    const event = new Event("ping");
    expect(event.target).toBe(null);
    expect(event.currentTarget).toBe(null);
  });

  test("coerces the type argument to a string", () => {
    expect(new Event(42).type).toBe("42");
  });

  test("requires a type argument", () => {
    expect(() => new Event()).toThrow(TypeError);
  });

  test("treats null and undefined init as an absent dictionary", () => {
    expect(new Event("ping", null).bubbles).toBe(false);
    expect(new Event("ping", undefined).bubbles).toBe(false);
  });

  test("rejects a non-object init dictionary", () => {
    expect(() => new Event("ping", 42)).toThrow(TypeError);
    expect(() => new Event("ping", "init")).toThrow(TypeError);
  });

  test("requires construction with new", () => {
    expect(() => Event("ping")).toThrow(TypeError);
  });

  test("supports subclassing with class syntax", () => {
    class ReadyEvent extends Event {
      constructor(type) {
        super(type);
        this.detail = "ready";
      }
    }

    const target = new EventTarget();
    let received = null;
    target.addEventListener("ready", (event) => {
      received = event;
    });

    const event = new ReadyEvent("ready");
    target.dispatchEvent(event);
    expect(received).toBe(event);
    expect(received instanceof ReadyEvent).toBe(true);
    expect(received instanceof Event).toBe(true);
    expect(received.type).toBe("ready");
    expect(received.detail).toBe("ready");
  });

  test("rejects incompatible receivers on its accessors", () => {
    const descriptor = Object.getOwnPropertyDescriptor(
      Event.prototype,
      "type"
    );
    expect(() => descriptor.get.call({})).toThrow(TypeError);
  });

  // Deliberate deviation from WebIDL, recorded in ADR 0104: this runtime's
  // accessors are non-enumerable, matching the pre-existing AbortSignal
  // `aborted` and `reason` accessors rather than WebIDL's enumerable
  // interface attributes.
  test("exposes its accessors as non-enumerable, unlike WebIDL", () => {
    expect(Object.keys(Event.prototype)).toEqual([]);

    for (const name of ["type", "bubbles", "cancelable", "defaultPrevented"]) {
      const descriptor = Object.getOwnPropertyDescriptor(Event.prototype, name);
      expect(typeof descriptor.get).toBe("function");
      expect(descriptor.enumerable).toBe(false);
      expect(descriptor.configurable).toBe(true);
    }
  });
});
