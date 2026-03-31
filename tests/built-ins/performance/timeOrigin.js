/*---
description: performance.timeOrigin
features: [performance]
---*/

describe("performance.timeOrigin", () => {
  test("is inherited from the Performance prototype", () => {
    const proto = Object.getPrototypeOf(performance);
    const ownDesc = Object.getOwnPropertyDescriptor(performance, "timeOrigin");
    const protoDesc = Object.getOwnPropertyDescriptor(proto, "timeOrigin");

    expect(ownDesc).toBeUndefined();
    expect(protoDesc).toBeDefined();
    expect(typeof protoDesc.get).toBe("function");
    expect(protoDesc.set).toBe(undefined);
    expect(protoDesc.enumerable).toBe(true);
    expect(protoDesc.configurable).toBe(true);
  });

  test("returns a positive number", () => {
    expect(typeof performance.timeOrigin).toBe("number");
    expect(performance.timeOrigin > 0).toBe(true);
  });

  test("is stable across reads while performance.now advances from it", () => {
    const firstOrigin = performance.timeOrigin;
    const secondOrigin = performance.timeOrigin;
    const firstNow = performance.now();
    const secondNow = performance.now();

    expect(firstOrigin).toBe(secondOrigin);
    expect(firstOrigin + secondNow >= firstOrigin + firstNow).toBe(true);
  });

  test("getter throws on incompatible receivers", () => {
    const proto = Object.getPrototypeOf(performance);
    const getter = Object.getOwnPropertyDescriptor(proto, "timeOrigin").get;

    expect(() => getter.call({})).toThrow(TypeError);
  });

});
