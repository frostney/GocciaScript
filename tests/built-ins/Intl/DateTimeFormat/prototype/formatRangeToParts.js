/*---
description: Intl.DateTimeFormat.prototype.formatRangeToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const isTemporal = typeof Temporal !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

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
    if (!hasFullICU) {
      expect(parts.every((part) => part.type === "literal")).toBe(true);
      return;
    }
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
    if (!hasFullICU) {
      expect(Array.isArray(parts)).toBe(true);
      return;
    }
    expect(parts.find((part) => part.type === "relatedYear" && part.source === "startRange").value).toBe("1999");
    expect(parts.find((part) => part.type === "relatedYear" && part.source === "endRange").value).toBe("1899");
  });
});

describe.runIf(isIntl && isTemporal)("Intl.DateTimeFormat.prototype.formatRangeToParts Temporal inputs", () => {
  test("localizes large Temporal.PlainDate range parts through ICU", () => {
    const dtf = new Intl.DateTimeFormat("fr-FR", {
      weekday: "long",
      year: "numeric",
      month: "long",
      day: "numeric"
    });
    const start = new Temporal.PlainDate(10000, 1, 1);
    const end = new Temporal.PlainDate(10000, 1, 2);
    const parts = dtf.formatRangeToParts(start, end);

    if (!hasFullICU) {
      expect(Array.isArray(parts)).toBe(true);
      return;
    }

    expect(parts.map((part) => part.value).join("")).toBe("samedi 1 – dimanche 2 janvier 10000");
    expect(parts.map((part) => part.type)).toEqual([
      "weekday",
      "literal",
      "day",
      "literal",
      "weekday",
      "literal",
      "day",
      "literal",
      "month",
      "literal",
      "year"
    ]);
    expect(parts.map((part) => part.source)).toEqual([
      "startRange",
      "startRange",
      "startRange",
      "shared",
      "endRange",
      "endRange",
      "endRange",
      "shared",
      "shared",
      "shared",
      "shared"
    ]);
  });

  test("emits parts for Temporal.PlainDate extremes without Date TimeClip", () => {
    const dtf = new Intl.DateTimeFormat("en", {
      weekday: "long",
      year: "numeric",
      month: "numeric",
      day: "numeric",
      calendar: "iso8601"
    });
    const start = new Temporal.PlainDate(-271821, 4, 19);
    const end = new Temporal.PlainDate(275760, 9, 13);
    const parts = dtf.formatRangeToParts(start, end);

    if (!hasFullICU) {
      expect(Array.isArray(parts)).toBe(true);
      return;
    }

    expect(parts.some((part) => part.type === "weekday" && part.source === "startRange")).toBe(true);
    expect(parts.some((part) => part.type === "weekday" && part.source === "endRange")).toBe(true);
    expect(parts.find((part) => part.type === "year" && part.source === "startRange").value).toBe("-271821");
    expect(parts.find((part) => part.type === "year" && part.source === "endRange").value).toBe("275760");
  });

  test("keeps Gregorian-cycle Temporal.PlainDate ranges uncollapsed", () => {
    const dtf = new Intl.DateTimeFormat("en-US", {
      year: "numeric",
      month: "short",
      day: "numeric"
    });
    const start = new Temporal.PlainDate(10000, 1, 1);
    const end = new Temporal.PlainDate(10400, 1, 1);
    const parts = dtf.formatRangeToParts(start, end);

    if (!hasFullICU) {
      expect(Array.isArray(parts)).toBe(true);
      return;
    }

    expect(parts.map((part) => part.value).join("")).toBe("Jan 1, 10000 – Jan 1, 10400");
    expect(parts.find((part) => part.type === "year" && part.source === "startRange").value).toBe("10000");
    expect(parts.find((part) => part.type === "year" && part.source === "endRange").value).toBe("10400");
  });

  test("emits era parts for mixed-era Temporal.PlainDate ranges", () => {
    const dtf = new Intl.DateTimeFormat("en", {
      calendar: "iso8601",
      era: "short",
      year: "numeric",
      month: "long",
      day: "numeric"
    });
    const start = new Temporal.PlainDate(-271821, 4, 19);
    const end = new Temporal.PlainDate(275760, 9, 13);
    const parts = dtf.formatRangeToParts(start, end);
    const formatted = parts.map((part) => part.value).join("");

    if (!hasFullICU) {
      expect(Array.isArray(parts)).toBe(true);
      return;
    }

    expect(formatted).toContain("BC");
    expect(formatted).toContain("AD");
    expect(formatted).toContain("-271821");
    expect(formatted).toContain("275760");
    expect(formatted).toContain("19");
    expect(formatted).toContain("13");
    expect(parts.find((part) => part.type === "era" && part.source === "startRange").value).toBe("BC");
    expect(parts.find((part) => part.type === "era" && part.source === "endRange").value).toBe("AD");
  });
});
