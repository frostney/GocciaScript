/*---
description: Date.prototype.toLocaleDateString delegates to Intl.DateTimeFormat date defaults
features: [Intl, Date]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Date.prototype.toLocaleDateString", () => {
  test("formats with date defaults", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC" };

    expect(date.toLocaleDateString("de-DE", options)).toBe(new Intl.DateTimeFormat("de-DE", {
      timeZone: "UTC",
      year: "numeric",
      month: "numeric",
      day: "numeric",
    }).format(date));
  });

  test("preserves explicit date options", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC", year: "numeric", month: "long" };

    expect(date.toLocaleDateString("en-US", options)).toBe(new Intl.DateTimeFormat("en-US", options).format(date));
  });
});
