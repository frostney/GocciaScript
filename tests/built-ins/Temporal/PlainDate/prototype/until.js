/*---
description: Temporal.PlainDate.prototype.until
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.until", () => {
  test("until() default returns days", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 1);
    const d2 = new Temporal.PlainDate(2024, 1, 11);
    const dur = d1.until(d2);
    expect(dur.days).toBe(10);
  });

  test("until() with largestUnit day", () => {
    const d1 = Temporal.PlainDate.from("2026-05-01");
    const d2 = Temporal.PlainDate.from("2026-07-30");
    expect(d1.until(d2, { largestUnit: "day" }).toString()).toBe("P90D");
  });

  test("until() with largestUnit months", () => {
    const d1 = Temporal.PlainDate.from("2026-05-01");
    const d2 = Temporal.PlainDate.from("2026-07-30");
    expect(d1.until(d2, { largestUnit: "months" }).toString()).toBe("P2M29D");
  });

  test("until() with largestUnit years on sub-year span", () => {
    const d1 = Temporal.PlainDate.from("2026-05-01");
    const d2 = Temporal.PlainDate.from("2026-07-30");
    expect(d1.until(d2, { largestUnit: "years" }).toString()).toBe("P2M29D");
  });

  test("until() with largestUnit weeks", () => {
    const d1 = Temporal.PlainDate.from("2026-05-01");
    const d2 = Temporal.PlainDate.from("2026-07-30");
    expect(d1.until(d2, { largestUnit: "weeks" }).toString()).toBe("P12W6D");
  });

  test("until() multi-year span with largestUnit years", () => {
    const d1 = Temporal.PlainDate.from("2020-01-15");
    const d2 = Temporal.PlainDate.from("2026-07-30");
    expect(d1.until(d2, { largestUnit: "years" }).toString()).toBe("P6Y6M15D");
  });

  test("until() negative result with largestUnit months", () => {
    const d1 = Temporal.PlainDate.from("2026-07-30");
    const d2 = Temporal.PlainDate.from("2026-05-01");
    expect(d1.until(d2, { largestUnit: "months" }).toString()).toBe("-P2M29D");
  });

  test("until() day clamping across months with largestUnit months", () => {
    const d1 = Temporal.PlainDate.from("2026-01-31");
    const d2 = Temporal.PlainDate.from("2026-03-28");
    expect(d1.until(d2, { largestUnit: "months" }).toString()).toBe("P1M28D");
  });

  test("until() same date returns zero duration", () => {
    const d = Temporal.PlainDate.from("2026-06-15");
    const dur = d.until(d, { largestUnit: "years" });
    expect(dur.years).toBe(0);
    expect(dur.months).toBe(0);
    expect(dur.days).toBe(0);
  });

  test("until() leap year boundary with largestUnit months", () => {
    const d1 = Temporal.PlainDate.from("2024-01-29");
    const d2 = Temporal.PlainDate.from("2024-03-01");
    expect(d1.until(d2, { largestUnit: "months" }).toString()).toBe("P1M1D");
  });
});
