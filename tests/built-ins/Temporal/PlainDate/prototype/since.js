/*---
description: Temporal.PlainDate.prototype.since
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.since", () => {
  test("since() default returns days", () => {
    const d1 = new Temporal.PlainDate(2024, 1, 11);
    const d2 = new Temporal.PlainDate(2024, 1, 1);
    const dur = d1.since(d2);
    expect(dur.days).toBe(10);
  });

  test("since() with largestUnit months", () => {
    const d1 = Temporal.PlainDate.from("2026-07-30");
    const d2 = Temporal.PlainDate.from("2026-05-01");
    expect(d1.since(d2, { largestUnit: "months" }).toString()).toBe("P2M29D");
  });

  test("since() with largestUnit weeks", () => {
    const d1 = Temporal.PlainDate.from("2026-07-30");
    const d2 = Temporal.PlainDate.from("2026-05-01");
    expect(d1.since(d2, { largestUnit: "weeks" }).toString()).toBe("P12W6D");
  });

  test("since() with largestUnit years across multi-year span", () => {
    const d1 = Temporal.PlainDate.from("2026-07-30");
    const d2 = Temporal.PlainDate.from("2020-01-15");
    expect(d1.since(d2, { largestUnit: "years" }).toString()).toBe("P6Y6M15D");
  });
});
