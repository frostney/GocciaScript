/*---
description: Intl.DateTimeFormat.prototype.resolvedOptions
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.resolvedOptions", () => {
  test("default includes calendar, numberingSystem, and timeZone", () => {
    const opts = new Intl.DateTimeFormat("en-US").resolvedOptions();
    expect(opts.locale).toBe("en-US");
    expect(opts.calendar).toBe("gregory");
    expect(opts.numberingSystem).toBe("latn");
    expect(opts.timeZone).toBe("UTC");
  });

  test("default resolves to year/month/day numeric", () => {
    const opts = new Intl.DateTimeFormat("en-US").resolvedOptions();
    expect(opts.year).toBe("numeric");
    expect(opts.month).toBe("numeric");
    expect(opts.day).toBe("numeric");
  });

  test("dateStyle excludes component properties", () => {
    const opts = new Intl.DateTimeFormat("en-US", {
      dateStyle: "full",
    }).resolvedOptions();
    expect(opts.dateStyle).toBe("full");
    expect(opts.year).toBe(undefined);
    expect(opts.month).toBe(undefined);
    expect(opts.day).toBe(undefined);
  });

  test("component properties exclude dateStyle/timeStyle", () => {
    const opts = new Intl.DateTimeFormat("en-US", {
      hour: "numeric",
      minute: "numeric",
    }).resolvedOptions();
    expect(opts.hour).toBe("numeric");
    expect(opts.minute).toBe("numeric");
    expect(opts.dateStyle).toBe(undefined);
    expect(opts.timeStyle).toBe(undefined);
  });

  test("explicit calendar option is preserved", () => {
    const opts = new Intl.DateTimeFormat("en-US", {
      calendar: "japanese",
    }).resolvedOptions();
    expect(opts.calendar).toBe("japanese");
  });

  test("timeZone option is preserved", () => {
    const opts = new Intl.DateTimeFormat("en-US", {
      timeZone: "America/New_York",
    }).resolvedOptions();
    expect(opts.timeZone).toBe("America/New_York");
  });
});
