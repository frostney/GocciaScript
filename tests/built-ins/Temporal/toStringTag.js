/*---
description: Temporal[Symbol.toStringTag]
features: [temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal[Symbol.toStringTag]", () => {
  test("Symbol.toStringTag is 'Temporal'", () => {
    expect(Temporal[Symbol.toStringTag]).toBe("Temporal");
  });

  test("Object.prototype.toString returns [object Temporal]", () => {
    expect(Object.prototype.toString.call(Temporal)).toBe("[object Temporal]");
  });

  test("Temporal.toString() returns [object Temporal]", () => {
    expect(Temporal.toString()).toBe("[object Temporal]");
  });

  test("Temporal.hasOwnProperty works", () => {
    expect(Temporal.hasOwnProperty("Now")).toBe(true);
    expect(Temporal.hasOwnProperty("Duration")).toBe(true);
    expect(Temporal.hasOwnProperty("PlainYearMonth")).toBe(true);
    expect(Temporal.hasOwnProperty("PlainMonthDay")).toBe(true);
    expect(Temporal.hasOwnProperty("ZonedDateTime")).toBe(true);
    expect(Temporal.hasOwnProperty("missing")).toBe(false);
  });
});
