/*---
description: Date.prototype.toLocaleTimeString delegates to Intl.DateTimeFormat time defaults
features: [Intl, Date]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Date.prototype.toLocaleTimeString", () => {
  test("formats with time defaults", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC" };

    expect(date.toLocaleTimeString("de-DE", options)).toBe(new Intl.DateTimeFormat("de-DE", {
      timeZone: "UTC",
      hour: "numeric",
      minute: "numeric",
      second: "numeric",
    }).format(date));
  });

  test("preserves explicit time options", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC", hour: "2-digit", minute: "2-digit" };

    expect(date.toLocaleTimeString("en-US", options)).toBe(new Intl.DateTimeFormat("en-US", options).format(date));
  });
});
