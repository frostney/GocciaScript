/*---
description: Temporal options bags for rounding, overflow, and toString precision
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal options bags", () => {

  describe("PlainTime.round options", () => {
    test("roundingMode ceil", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 500);
      const rounded = t.round({ smallestUnit: "second", roundingMode: "ceil" });
      expect(rounded.second).toBe(31);
    });

    test("roundingMode floor", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 500);
      const rounded = t.round({ smallestUnit: "second", roundingMode: "floor" });
      expect(rounded.second).toBe(30);
    });

    test("roundingMode trunc", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 999);
      const rounded = t.round({ smallestUnit: "second", roundingMode: "trunc" });
      expect(rounded.second).toBe(30);
    });

    test("roundingIncrement", () => {
      const t = new Temporal.PlainTime(13, 47, 0);
      const rounded = t.round({ smallestUnit: "minute", roundingIncrement: 15 });
      expect(rounded.minute).toBe(45);
    });
  });

  describe("PlainDateTime.round options", () => {
    test("roundingMode floor", () => {
      const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30, 500);
      const rounded = dt.round({ smallestUnit: "second", roundingMode: "floor" });
      expect(rounded.second).toBe(30);
      expect(rounded.millisecond).toBe(0);
    });

    test("roundingMode ceil", () => {
      const dt = new Temporal.PlainDateTime(2024, 3, 15, 13, 45, 30, 1);
      const rounded = dt.round({ smallestUnit: "second", roundingMode: "ceil" });
      expect(rounded.second).toBe(31);
    });
  });

  describe("Instant.round options", () => {
    test("roundingMode trunc", () => {
      const inst = Temporal.Instant.fromEpochMilliseconds(1710510330999);
      const rounded = inst.round({ smallestUnit: "second", roundingMode: "trunc" });
      expect(rounded.epochMilliseconds).toBe(1710510330000);
    });
  });

  describe("Duration.round", () => {
    test("round time units", () => {
      const d = new Temporal.Duration(0, 0, 0, 0, 25, 30);
      const rounded = d.round({ largestUnit: "day" });
      expect(rounded.days).toBe(1);
      expect(rounded.hours).toBe(1);
      expect(rounded.minutes).toBe(30);
    });

    test("round to hours", () => {
      const d = new Temporal.Duration(0, 0, 0, 0, 1, 45);
      const rounded = d.round({ smallestUnit: "hour", roundingMode: "halfExpand" });
      expect(rounded.hours).toBe(2);
    });

    test("round floor", () => {
      const d = new Temporal.Duration(0, 0, 0, 0, 1, 59);
      const rounded = d.round({ smallestUnit: "hour", roundingMode: "floor" });
      expect(rounded.hours).toBe(1);
    });
  });

  describe("PlainDate.from overflow", () => {
    test("constrain clamps day", () => {
      const d = Temporal.PlainDate.from({ year: 2024, month: 2, day: 30 }, { overflow: "constrain" });
      expect(d.day).toBe(29);
    });

    test("reject throws on invalid day", () => {
      expect(() => {
        Temporal.PlainDate.from({ year: 2024, month: 2, day: 30 }, { overflow: "reject" });
      }).toThrow(RangeError);
    });
  });

  describe("toString fractionalSecondDigits", () => {
    test("PlainTime with 0 digits", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 123);
      const s = t.toString({ fractionalSecondDigits: 0 });
      expect(s).toBe("13:45:30");
    });

    test("PlainTime with 3 digits", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 123, 456);
      const s = t.toString({ fractionalSecondDigits: 3 });
      expect(s).toBe("13:45:30.123");
    });

    test("PlainTime with 6 digits", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 123, 456, 789);
      const s = t.toString({ fractionalSecondDigits: 6 });
      expect(s).toBe("13:45:30.123456");
    });

    test("PlainTime with 9 digits", () => {
      const t = new Temporal.PlainTime(13, 45, 30, 123, 456, 789);
      const s = t.toString({ fractionalSecondDigits: 9 });
      expect(s).toBe("13:45:30.123456789");
    });

    test("PlainTime auto hides trailing zeros", () => {
      const t = new Temporal.PlainTime(13, 45, 30);
      const s = t.toString({ fractionalSecondDigits: "auto" });
      expect(s).toBe("13:45:30");
    });
  });

  describe("Temporal.Now.timeZoneId", () => {
    test("returns a string", () => {
      const tz = Temporal.Now.timeZoneId();
      expect(typeof tz).toBe("string");
      expect(tz.length).toBeGreaterThan(0);
    });
  });
});
