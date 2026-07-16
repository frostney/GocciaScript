/*---
description: Temporal.PlainDate.prototype.subtract
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.subtract", () => {
  test("subtract()", () => {
    const d = new Temporal.PlainDate(2024, 3, 15);
    const result = d.subtract(new Temporal.Duration(0, 0, 0, 15));
    expect(result.toString()).toBe("2024-02-29");
  });

  test("accepts strings and duration-like objects", () => {
    const date = new Temporal.PlainDate(2024, 3, 15);
    expect(date.subtract("P15D").toString()).toBe("2024-02-29");
    expect(date.subtract({ months: 1 }).toString()).toBe("2024-02-15");
  });

  test("honors overflow options", () => {
    const date = new Temporal.PlainDate(2024, 3, 31);
    expect(date.subtract({ months: 1 }).toString()).toBe("2024-02-29");
    expect(() => date.subtract({ months: 1 }, { overflow: "reject" })).toThrow(RangeError);
  });

  test("rejects invalid duration inputs and incompatible receivers", () => {
    const subtract = Temporal.PlainDate.prototype.subtract;
    expect(() => new Temporal.PlainDate(2024, 1, 1).subtract("not-a-duration")).toThrow(RangeError);
    expect(() => new Temporal.PlainDate(2024, 1, 1).subtract(1)).toThrow(TypeError);
    expect(() => subtract.call({}, { days: 1 })).toThrow(TypeError);
  });
});
