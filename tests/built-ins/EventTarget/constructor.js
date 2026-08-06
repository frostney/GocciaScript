/*---
description: EventTarget constructor behavior
features: [EventTarget, AbortSignal]
---*/

describe("EventTarget constructor", () => {
  test("is exposed as a runtime global", () => {
    expect(typeof EventTarget).toBe("function");
    expect(Goccia.runtimeGlobals.includes("EventTarget")).toBe(true);
  });

  test("creates an EventTarget instance", () => {
    const target = new EventTarget();
    expect(target instanceof EventTarget).toBe(true);
    expect(Object.prototype.toString.call(target)).toBe("[object EventTarget]");
  });

  test("requires construction with new", () => {
    expect(() => EventTarget()).toThrow(TypeError);
  });

  test("exposes the listener methods on its prototype", () => {
    expect(typeof EventTarget.prototype.addEventListener).toBe("function");
    expect(typeof EventTarget.prototype.removeEventListener).toBe("function");
    expect(typeof EventTarget.prototype.dispatchEvent).toBe("function");
  });

  test("supports subclassing with class syntax", () => {
    class Emitter extends EventTarget {
      constructor() {
        super();
        this.tag = "emitter";
      }
    }

    const emitter = new Emitter();
    expect(emitter instanceof Emitter).toBe(true);
    expect(emitter instanceof EventTarget).toBe(true);
    expect(emitter.tag).toBe("emitter");

    let calls = 0;
    emitter.addEventListener("go", () => {
      calls += 1;
    });
    expect(emitter.dispatchEvent(new Event("go"))).toBe(true);
    expect(calls).toBe(1);
  });

  test("AbortSignal inherits from EventTarget", () => {
    const controller = new AbortController();
    expect(controller.signal instanceof EventTarget).toBe(true);
    expect(AbortSignal.prototype instanceof EventTarget).toBe(true);
    expect(Object.getPrototypeOf(AbortSignal.prototype)).toBe(
      EventTarget.prototype
    );
    expect(Object.getPrototypeOf(AbortSignal)).toBe(EventTarget);
  });
});
