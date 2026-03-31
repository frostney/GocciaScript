/*---
description: performance.toJSON
features: [performance]
---*/

describe("performance.toJSON", () => {
  test("is inherited from the Performance prototype", () => {
    const proto = Object.getPrototypeOf(performance);
    const desc = Object.getOwnPropertyDescriptor(proto, "toJSON");

    expect(performance.hasOwnProperty("toJSON")).toBe(false);
    expect(proto.hasOwnProperty("toJSON")).toBe(true);
    expect(performance.toJSON).toBe(proto.toJSON);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(desc.writable).toBe(true);
  });

  test("returns a JSON snapshot with timeOrigin", () => {
    const json = performance.toJSON();
    const keys = Object.keys(json);

    expect(keys.includes("timeOrigin")).toBe(true);
    expect(json.timeOrigin).toBe(performance.timeOrigin);
  });

  test("does not include methods in the JSON snapshot", () => {
    const json = performance.toJSON();

    expect(json.now).toBe(undefined);
    expect(json.toJSON).toBe(undefined);
  });

  test("JSON.stringify(performance) uses the timeOrigin snapshot", () => {
    const json = JSON.parse(JSON.stringify(performance));

    expect("timeOrigin" in json).toBe(true);
    expect(json.timeOrigin).toBe(performance.timeOrigin);
  });

  test("throws on incompatible receivers", () => {
    const proto = Object.getPrototypeOf(performance);

    expect(() => proto.toJSON.call({})).toThrow(TypeError);
  });
});
