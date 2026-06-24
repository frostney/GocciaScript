/*---
description: Temporal.PlainDate.prototype.calendarId
features: [Temporal]
---*/

const isTemporal = typeof Temporal !== "undefined";

describe.runIf(isTemporal)("Temporal.PlainDate.prototype.calendarId", () => {
  test("calendarId is iso8601", () => {
    const d = new Temporal.PlainDate(2024, 1, 1);
    expect(d.calendarId).toBe("iso8601");
  });

  test("constructor stores non-ISO calendarId", () => {
    const d = new Temporal.PlainDate(2000, 3, 6, "gregory");
    expect(d.calendarId).toBe("gregory");
  });

  test("withCalendar stores canonical calendarId", () => {
    const d = new Temporal.PlainDate(1911, 10, 10);
    expect(d.withCalendar("roc").calendarId).toBe("roc");
    expect(d.withCalendar("islamicc").calendarId).toBe("islamic-civil");
  });
});
