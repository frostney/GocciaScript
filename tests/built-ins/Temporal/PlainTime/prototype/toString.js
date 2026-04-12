/*---
description: Temporal.PlainTime.prototype.toString
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainTime.prototype.toString", () => {
  test("toString()", () => {
    expect(new Temporal.PlainTime(13, 45, 30).toString()).toBe("13:45:30");
    expect(new Temporal.PlainTime(9, 5, 0).toString()).toBe("09:05:00");
    expect(new Temporal.PlainTime(0, 0, 0, 500).toString()).toBe("00:00:00.500");
  });

  test("fractionalSecondDigits 0", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 123);
    const s = t.toString({ fractionalSecondDigits: 0 });
    expect(s).toBe("13:45:30");
  });

  test("fractionalSecondDigits 3", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 123, 456);
    const s = t.toString({ fractionalSecondDigits: 3 });
    expect(s).toBe("13:45:30.123");
  });

  test("fractionalSecondDigits 6", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 123, 456, 789);
    const s = t.toString({ fractionalSecondDigits: 6 });
    expect(s).toBe("13:45:30.123456");
  });

  test("fractionalSecondDigits 9", () => {
    const t = new Temporal.PlainTime(13, 45, 30, 123, 456, 789);
    const s = t.toString({ fractionalSecondDigits: 9 });
    expect(s).toBe("13:45:30.123456789");
  });

  test("fractionalSecondDigits auto hides trailing zeros", () => {
    const t = new Temporal.PlainTime(13, 45, 30);
    const s = t.toString({ fractionalSecondDigits: "auto" });
    expect(s).toBe("13:45:30");
  });
});
