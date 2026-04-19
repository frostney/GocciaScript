/*---
description: Symbol arguments to Temporal methods throw TypeError (ES2026 §7.1.17 ToString)
features: [Symbol, Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Symbol coercion in Temporal option values", () => {
  test("Duration.prototype.total with Symbol unit throws TypeError", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(() => d.total({ unit: Symbol("hours") })).toThrow(TypeError);
  });

  test("Duration.prototype.total with Symbol string arg throws TypeError", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(() => d.total(Symbol("hours"))).toThrow(TypeError);
  });

  test("Duration.prototype.round with Symbol smallestUnit throws TypeError", () => {
    const d = new Temporal.Duration(0, 0, 0, 0, 1, 30);
    expect(() => d.round({ smallestUnit: Symbol("minutes") })).toThrow(TypeError);
  });

});

describe.runIf(isTemporal)("Symbol coercion in Temporal timeZone arguments", () => {
  test("PlainDate.prototype.toZonedDateTime with Symbol timeZone throws TypeError", () => {
    const pd = Temporal.PlainDate.from("2024-01-15");
    expect(() => pd.toZonedDateTime(Symbol("UTC"))).toThrow(TypeError);
  });

  test("PlainDate.prototype.toZonedDateTime with Symbol in options throws TypeError", () => {
    const pd = Temporal.PlainDate.from("2024-01-15");
    expect(() => pd.toZonedDateTime({ timeZone: Symbol("UTC") })).toThrow(TypeError);
  });

  test("PlainDateTime.prototype.toZonedDateTime with Symbol timeZone throws TypeError", () => {
    const pdt = Temporal.PlainDateTime.from("2024-01-15T10:30:00");
    expect(() => pdt.toZonedDateTime(Symbol("UTC"))).toThrow(TypeError);
  });

  test("PlainDateTime.prototype.toZonedDateTime with Symbol in options throws TypeError", () => {
    const pdt = Temporal.PlainDateTime.from("2024-01-15T10:30:00");
    expect(() => pdt.toZonedDateTime({ timeZone: Symbol("UTC") })).toThrow(TypeError);
  });

  test("ZonedDateTime.prototype.withTimeZone with Symbol throws TypeError", () => {
    const zdt = Temporal.ZonedDateTime.from("2024-01-15T10:30:00+00:00[UTC]");
    expect(() => zdt.withTimeZone(Symbol("UTC"))).toThrow(TypeError);
  });
});

describe.runIf(isTemporal)("Symbol coercion in Temporal.PlainMonthDay", () => {
  test("PlainMonthDay.from with Symbol monthCode throws TypeError", () => {
    expect(() => Temporal.PlainMonthDay.from({ monthCode: Symbol("M07"), day: 15 })).toThrow(TypeError);
  });
});

describe.runIf(isTemporal)("Symbol coercion in Temporal.PlainYearMonth", () => {
  test("PlainYearMonth.from with Symbol monthCode throws TypeError", () => {
    expect(() => Temporal.PlainYearMonth.from({ year: 2024, monthCode: Symbol("M07") })).toThrow(TypeError);
  });
});

describe.runIf(isTemporal)("Symbol coercion in Temporal.Now", () => {
  test("Temporal.Now.zonedDateTimeISO with Symbol timeZone throws TypeError", () => {
    expect(() => Temporal.Now.zonedDateTimeISO(Symbol("UTC"))).toThrow(TypeError);
  });
});
