/*---
description: Intl.NumberFormat.prototype.formatRangeToParts
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

describe.runIf(isIntl)("Intl.NumberFormat.prototype.formatRangeToParts", () => {
  test("formatRangeToParts is exposed", () => {
    const nf = new Intl.NumberFormat("en-US");
    expect(typeof nf.formatRangeToParts).toBe("function");
  });

  test("emits source-tagged parts for a currency range", () => {
    const nf = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      maximumFractionDigits: 0
    });
    const parts = nf.formatRangeToParts(3, 5);
    if (!hasFullICU) {
      expect(parts.map((part) => part.value).join("")).toBe(nf.formatRange(3, 5));
      return;
    }
    expect(parts.map((part) => part.source)).toEqual([
      "startRange",
      "startRange",
      "shared",
      "endRange",
      "endRange"
    ]);
    expect(parts.map((part) => part.type)).toEqual([
      "currency",
      "integer",
      "literal",
      "currency",
      "integer"
    ]);
    expect(parts.map((part) => part.value).join("")).toBe("$3 – $5");
  });

  test("emits shared approximate parts when rounded endpoints match", () => {
    const nf = new Intl.NumberFormat("en-US", {
      style: "currency",
      currency: "USD",
      maximumFractionDigits: 0
    });
    const parts = nf.formatRangeToParts(2.9, 3.1);
    if (!hasFullICU) {
      expect(parts.map((part) => part.value).join("")).toBe(nf.formatRange(2.9, 3.1));
      return;
    }
    expect(parts.map((part) => part.source)).toEqual(["shared", "shared", "shared"]);
    expect(parts.map((part) => part.type)).toEqual(["approximatelySign", "currency", "integer"]);
    expect(parts.map((part) => part.value).join("")).toBe("~$3");
  });
});
