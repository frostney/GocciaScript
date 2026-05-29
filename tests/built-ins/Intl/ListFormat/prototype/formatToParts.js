/*---
description: Intl.ListFormat.prototype.formatToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

const partsString = (parts) => parts.map((part) => part.value).join("");

describe.runIf(isIntl && typeof Intl.ListFormat !== "undefined")("Intl.ListFormat.prototype.formatToParts", () => {
  test("decomposes a conjunction list into element and literal parts", () => {
    const lf = new Intl.ListFormat("en-US", { style: "long", type: "conjunction" });
    const parts = lf.formatToParts(["A", "B", "C"]);

    expect(parts).toEqual([
      { type: "element", value: "A" },
      { type: "literal", value: ", " },
      { type: "element", value: "B" },
      { type: "literal", value: ", and " },
      { type: "element", value: "C" },
    ]);
    expect(partsString(parts)).toBe(lf.format(["A", "B", "C"]));
  });

  test("uses locale-specific disjunction literals", () => {
    const lf = new Intl.ListFormat("en-US", { style: "long", type: "disjunction" });
    const parts = lf.formatToParts(["A", "B", "C"]);

    expect(parts.map((part) => part.value)).toEqual(["A", ", ", "B", ", or ", "C"]);
    expect(parts.map((part) => part.type)).toEqual(["element", "literal", "element", "literal", "element"]);
  });

  test("does not confuse element text with list separators", () => {
    const lf = new Intl.ListFormat("en-US", { style: "long", type: "conjunction" });
    const parts = lf.formatToParts(["A", "and"]);

    expect(parts).toEqual([
      { type: "element", value: "A" },
      { type: "literal", value: " and " },
      { type: "element", value: "and" },
    ]);
  });
});
