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
});
