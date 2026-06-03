/*---
description: Date.prototype.toLocaleString delegates to Intl.DateTimeFormat date and time defaults
features: [Intl, Date]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Date.prototype.toLocaleString", () => {
  test("formats with date and time defaults", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC" };

    expect(date.toLocaleString("de-DE", options)).toBe(new Intl.DateTimeFormat("de-DE", {
      timeZone: "UTC",
      year: "numeric",
      month: "numeric",
      day: "numeric",
      hour: "numeric",
      minute: "numeric",
      second: "numeric",
    }).format(date));
  });

  test("preserves explicit date and time options", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC", month: "long", day: "numeric", hour: "2-digit", minute: "2-digit" };

    expect(date.toLocaleString("en-US", options)).toBe(new Intl.DateTimeFormat("en-US", options).format(date));
  });

  test("preserves style-only options without adding component defaults", () => {
    const date = new Date(Date.UTC(2026, 0, 1, 15, 30, 45));
    const options = { timeZone: "UTC", dateStyle: "short" };

    expect(date.toLocaleString("en-US", options)).toBe(new Intl.DateTimeFormat("en-US", options).format(date));
  });

  test("throws TypeError for fake Date receiver", () => {
    const fakeDate = Object.create(Date.prototype);

    expect(() => Date.prototype.toLocaleString.call(fakeDate)).toThrow(TypeError);
  });
});
