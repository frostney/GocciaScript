/*---
description: Intl.RelativeTimeFormat.prototype.formatToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

const partsString = (parts) => parts.map((part) => part.value).join("");

describe.runIf(isIntl && typeof Intl.RelativeTimeFormat !== "undefined")("Intl.RelativeTimeFormat.prototype.formatToParts", () => {
  test("decomposes the numeric value and annotates it with the unit", () => {
    const rtf = new Intl.RelativeTimeFormat("en-US");
    const parts = rtf.formatToParts(-1, "day");

    expect(parts[0].type).toBe("integer");
    expect(parts[0].value).toBe("1");
    expect(parts[0].unit).toBe("day");
    expect(parts[1].type).toBe("literal");
    expect(parts[1].unit).toBe(undefined);
    expect(partsString(parts)).toBe(rtf.format(-1, "day"));
  });

  test("decomposes fractional numeric values", () => {
    const rtf = new Intl.RelativeTimeFormat("en-US");
    const parts = rtf.formatToParts(1234.5, "day");
    const numericParts = parts.filter((part) => part.type !== "literal");

    expect(numericParts.map((part) => part.type)).toEqual(["integer", "group", "integer", "decimal", "fraction"]);
    expect(numericParts.map((part) => part.value)).toEqual(["1", ",", "234", ".", "5"]);
    expect(numericParts.every((part) => part.unit === "day")).toBe(true);
    expect(partsString(parts)).toBe(rtf.format(1234.5, "day"));
  });

  test("decomposes numbers with the resolved numbering system", () => {
    const rtf = new Intl.RelativeTimeFormat("en-US", { numberingSystem: "arab" });
    const numericParts = rtf.formatToParts(-1234, "day").filter((part) => part.type !== "literal");

    expect(numericParts.map((part) => part.type)).toEqual(["integer", "group", "integer"]);
    expect(numericParts.map((part) => part.value)).toEqual(["\u0661", "\u066c", "\u0662\u0663\u0664"]);
    expect(numericParts.every((part) => part.unit === "day")).toBe(true);
  });

  test("returns literal relative names for numeric auto", () => {
    const rtf = new Intl.RelativeTimeFormat("en-US", { numeric: "auto" });
    const parts = rtf.formatToParts(-1, "day");

    expect(parts).toEqual([{ type: "literal", value: "yesterday" }]);
    expect(partsString(parts)).toBe(rtf.format(-1, "day"));
  });
});
