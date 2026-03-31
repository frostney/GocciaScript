/*---
description: performance[Symbol.toStringTag]
features: [performance]
---*/

describe("performance[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag is 'Performance'", () => {
    expect(performance[Symbol.toStringTag]).toBe("Performance");
  });

  test("Symbol.toStringTag is inherited from the Performance prototype", () => {
    const proto = Object.getPrototypeOf(performance);
    const desc = Object.getOwnPropertyDescriptor(proto, Symbol.toStringTag);

    expect(performance.hasOwnProperty(Symbol.toStringTag)).toBe(false);
    expect(proto.hasOwnProperty(Symbol.toStringTag)).toBe(true);
    expect(desc.value).toBe("Performance");
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
    expect(desc.writable).toBe(false);
  });

  test("Object.prototype.toString returns [object Performance]", () => {
    expect(Object.prototype.toString.call(performance)).toBe("[object Performance]");
  });

  test("performance.toString() returns [object Performance]", () => {
    expect(performance.toString()).toBe("[object Performance]");
  });

  test("performance.hasOwnProperty only reports own properties", () => {
    expect(performance.hasOwnProperty("now")).toBe(false);
    expect(performance.hasOwnProperty("timeOrigin")).toBe(false);
    expect(performance.hasOwnProperty("toJSON")).toBe(false);
    expect(performance.hasOwnProperty("missing")).toBe(false);
  });
});
