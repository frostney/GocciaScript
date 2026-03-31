/*---
description: performance.now
features: [performance]
---*/

describe("performance.now", () => {
  test("performance is exposed as an object instance", () => {
    expect(typeof performance).toBe("object");
  });

  test("performance.now is inherited from the Performance prototype", () => {
    const proto = Object.getPrototypeOf(performance);
    const desc = Object.getOwnPropertyDescriptor(proto, "now");

    expect(proto).not.toBe(Object.prototype);
    expect(performance.hasOwnProperty("now")).toBe(false);
    expect(proto.hasOwnProperty("now")).toBe(true);
    expect(performance.now).toBe(proto.now);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.writable).toBe(true);
  });

  test("returns a non-negative number", () => {
    const value = performance.now();

    expect(typeof value).toBe("number");
    expect(value >= 0).toBe(true);
  });

  test("is monotonic across successive calls", () => {
    const first = performance.now();
    const second = performance.now();
    const third = performance.now();

    expect(second >= first).toBe(true);
    expect(third >= second).toBe(true);
  });

  test("globalThis.performance is identical", () => {
    expect(globalThis.performance === performance).toBe(true);
  });

  test("throws on incompatible receivers", () => {
    const proto = Object.getPrototypeOf(performance);

    expect(() => proto.now.call({})).toThrow(TypeError);
  });
});
