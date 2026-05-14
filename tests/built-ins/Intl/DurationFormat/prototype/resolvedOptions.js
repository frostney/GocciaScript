/*---
description: Intl.DurationFormat.prototype.resolvedOptions
features: [Intl]
---*/

describe("Intl.DurationFormat.prototype.resolvedOptions", () => {
  test("default includes numberingSystem", () => {
    const opts = new Intl.DurationFormat("en-US").resolvedOptions();
    expect(opts.locale).toBe("en-US");
    expect(opts.numberingSystem).toBe("latn");
    expect(opts.style).toBe("short");
  });

  test("display fields default to auto", () => {
    const opts = new Intl.DurationFormat("en-US").resolvedOptions();
    expect(opts.yearsDisplay).toBe("auto");
    expect(opts.monthsDisplay).toBe("auto");
    expect(opts.weeksDisplay).toBe("auto");
    expect(opts.daysDisplay).toBe("auto");
    expect(opts.hoursDisplay).toBe("auto");
    expect(opts.minutesDisplay).toBe("auto");
    expect(opts.secondsDisplay).toBe("auto");
    expect(opts.millisecondsDisplay).toBe("auto");
    expect(opts.microsecondsDisplay).toBe("auto");
    expect(opts.nanosecondsDisplay).toBe("auto");
  });
});
