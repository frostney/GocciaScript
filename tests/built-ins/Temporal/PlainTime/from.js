/*---
description: Temporal.PlainTime.from
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.from", () => {
  test("from() with string", () => {
    const t = Temporal.PlainTime.from("13:45:30");
    expect(t.hour).toBe(13);
    expect(t.minute).toBe(45);
    expect(t.second).toBe(30);
  });

  test("from() with object", () => {
    const t = Temporal.PlainTime.from({ hour: 10, minute: 30 });
    expect(t.hour).toBe(10);
    expect(t.minute).toBe(30);
  });
});

describe("Temporal.PlainTime.from non-finite fields", () => {
  test("hour Infinity throws RangeError", () => {
    expect(() => Temporal.PlainTime.from({ hour: Infinity })).toThrow(RangeError);
  });

  test("minute NaN throws RangeError", () => {
    expect(() => Temporal.PlainTime.from({ hour: 1, minute: NaN })).toThrow(RangeError);
  });
});
