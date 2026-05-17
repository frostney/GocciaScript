/*---
description: Intl.DateTimeFormat.prototype.formatRangeToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.DateTimeFormat.prototype.formatRangeToParts", () => {
  test("formatRangeToParts is exposed", () => {
    const dtf = new Intl.DateTimeFormat("en-US");
    expect(typeof dtf.formatRangeToParts).toBe("function");
  });

  test("emits source-tagged parts for a collapsed date range", () => {
    const dtf = new Intl.DateTimeFormat("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric",
      timeZone: "UTC"
    });
    const parts = dtf.formatRangeToParts(Date.UTC(2026, 0, 1), Date.UTC(2026, 0, 15));
    expect(parts.map((part) => part.value).join("")).toBe(dtf.formatRange(Date.UTC(2026, 0, 1), Date.UTC(2026, 0, 15)));
    expect(parts.map((part) => part.type)).toEqual([
      "month",
      "literal",
      "day",
      "literal",
      "day",
      "literal",
      "year"
    ]);
    expect(parts.map((part) => part.source)).toEqual([
      "shared",
      "shared",
      "startRange",
      "shared",
      "endRange",
      "shared",
      "shared"
    ]);
  });

  test("emits relatedYear parts for non-Gregorian calendar ranges", () => {
    const dtf = new Intl.DateTimeFormat("en-US-u-ca-chinese", { timeZone: "UTC" });
    const parts = dtf.formatRangeToParts(
      new Date("2000-01-01T00:00Z"),
      new Date("1900-01-01T00:00Z")
    );
    expect(parts.find((part) => part.type === "relatedYear" && part.source === "startRange").value).toBe("1999");
    expect(parts.find((part) => part.type === "relatedYear" && part.source === "endRange").value).toBe("1899");
  });
});
