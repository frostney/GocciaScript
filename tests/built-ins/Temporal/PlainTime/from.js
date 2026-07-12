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

  test("from ignores valid calendar annotations", () => {
    const values = [
      "12:34:56.987654321[u-ca=unknown]",
      "T12:34:56.987654321[UTC][!u-ca=unknown]",
      "1970-01-01T12:34:56.987654321[u-ca=iso8601][u-ca=discord]"
    ];
    for (const value of values) {
      const time = Temporal.PlainTime.from(value);
      expect(time.toString()).toBe("12:34:56.987654321");
    }
  });
});

describe.runIf(isTemporal)("Temporal.PlainTime.from non-finite fields", () => {
  test("hour Infinity throws RangeError", () => {
    expect(() => Temporal.PlainTime.from({ hour: Infinity })).toThrow(RangeError);
  });

  test("minute NaN throws RangeError", () => {
    expect(() => Temporal.PlainTime.from({ hour: 1, minute: NaN })).toThrow(RangeError);
  });
});

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.add large duration components", () => {
  test("hours beyond Int32 wrap correctly into the clock", () => {
    const t = Temporal.PlainTime.from({ hour: 0 }).add({ hours: 3000000001 });
    expect(t.hour).toBe(1);
  });
});
