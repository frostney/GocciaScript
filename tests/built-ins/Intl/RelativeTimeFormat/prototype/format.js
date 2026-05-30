/*---
description: Intl.RelativeTimeFormat.prototype.format
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl && typeof Intl.RelativeTimeFormat !== "undefined")("Intl.RelativeTimeFormat.prototype.format", () => {
  test("formats numeric relative values with localized direction and plural-sensitive units", () => {
    const rtf = new Intl.RelativeTimeFormat("en-US", { numeric: "always" });

    expect(rtf.format(-1, "day")).toBe("1 day ago");
    expect(rtf.format(1, "day")).toBe("in 1 day");
    expect(rtf.format(-2, "day")).toBe("2 days ago");
    expect(rtf.format(1000, "day")).toBe("in 1,000 days");
  });

  test("formats numeric auto names when CLDR defines a relative literal", () => {
    const rtf = new Intl.RelativeTimeFormat("en-US", { numeric: "auto" });

    expect(rtf.format(-1, "day")).toBe("yesterday");
    expect(rtf.format(0, "day")).toBe("today");
    expect(rtf.format(1, "day")).toBe("tomorrow");
    expect(rtf.format(-2, "day")).toBe("2 days ago");
  });

  test("formats style-specific relative units", () => {
    const shortFormat = new Intl.RelativeTimeFormat("en-US", { style: "short" });
    const narrowFormat = new Intl.RelativeTimeFormat("en-US", { style: "narrow" });

    expect(shortFormat.format(2, "quarter")).toBe("in 2 qtrs.");
    expect(narrowFormat.format(-2, "quarter")).toBe("2 q ago");
  });
});
