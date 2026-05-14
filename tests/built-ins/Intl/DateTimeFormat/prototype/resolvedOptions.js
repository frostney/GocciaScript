/*---
description: Intl.DateTimeFormat.prototype.resolvedOptions
features: [Intl]
---*/

describe("Intl.DateTimeFormat.prototype.resolvedOptions", () => {
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

  test("normalizes offset time zones to HH:MM form", () => {
    const cases = [
      ["+03", "+03:00"],
      ["+13", "+13:00"],
      ["+23", "+23:00"],
      ["-07", "-07:00"],
      ["-14", "-14:00"],
      ["-21", "-21:00"],
      ["+01:03", "+01:03"],
      ["+15:59", "+15:59"],
      ["+22:27", "+22:27"],
      ["-02:32", "-02:32"],
      ["-17:01", "-17:01"],
      ["-22:23", "-22:23"],
    ];

    for (const [timeZone, expected] of cases) {
      const options = new Intl.DateTimeFormat("en", { timeZone }).resolvedOptions();
      expect(options.timeZone).toBe(expected);
    }
  });

  test("rejects invalid offset time zones", () => {
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "+24" })).toThrow(RangeError);
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "+01:60" })).toThrow(RangeError);
    expect(() => new Intl.DateTimeFormat("en", { timeZone: "+01:02:03" })).toThrow(RangeError);
  });
});
