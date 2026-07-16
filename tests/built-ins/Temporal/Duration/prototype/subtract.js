/*---
description: Temporal.Duration.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.Duration.prototype.subtract", () => {
  test("subtract()", () => {
    const d1 = new Temporal.Duration(0, 0, 0, 10);
    const d2 = new Temporal.Duration(0, 0, 0, 3);
    const diff = d1.subtract(d2);
    expect(diff.days).toBe(7);
  });

  test("accepts strings and duration-like objects", () => {
    const duration = Temporal.Duration.from({ seconds: 90 });
    expect(duration.subtract("PT30S").seconds).toBe(60);
    expect(duration.subtract({ seconds: 45 }).seconds).toBe(45);
  });

  test("can produce a negative result", () => {
    expect(Temporal.Duration.from({ seconds: 1 }).subtract({ seconds: 2 }).seconds).toBe(-1);
  });

  test("rejects invalid duration inputs and incompatible receivers", () => {
    const subtract = Temporal.Duration.prototype.subtract;
    expect(() => Temporal.Duration.from({ seconds: 1 }).subtract("not-a-duration")).toThrow(RangeError);
    expect(() => Temporal.Duration.from({ seconds: 1 }).subtract(1)).toThrow(TypeError);
    expect(() => subtract.call({}, { seconds: 1 })).toThrow(TypeError);
  });
});
