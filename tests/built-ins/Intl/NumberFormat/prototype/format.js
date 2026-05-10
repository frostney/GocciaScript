/*---
description: Intl.NumberFormat.prototype.format
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.NumberFormat.prototype.format", () => {
  test("format returns a string", () => {
    const nf = new Intl.NumberFormat("en-US");
    const result = nf.format(1234.56);
    expect(typeof result).toBe("string");
  });

  test("format includes grouping separator for large numbers", () => {
    const nf = new Intl.NumberFormat("en-US");
    const result = nf.format(1234567);
    expect(result.includes(",")).toBe(true);
  });

  test("currency format produces a string containing the currency symbol", () => {
    const nf = new Intl.NumberFormat("en-US", { style: "currency", currency: "USD" });
    const result = nf.format(9.99);
    expect(result.includes("$")).toBe(true);
  });

  test("percent format produces a string containing the percent sign", () => {
    const nf = new Intl.NumberFormat("en-US", { style: "percent" });
    const result = nf.format(0.75);
    expect(result.includes("%")).toBe(true);
  });
});
