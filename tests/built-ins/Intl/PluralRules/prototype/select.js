/*---
description: Intl.PluralRules.prototype.select
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.PluralRules.prototype.select", () => {
  test("select(0) returns 'other' for English", () => {
    const pr = new Intl.PluralRules("en");
    expect(pr.select(0)).toBe("other");
  });

  test("select(1) returns 'one' for English", () => {
    const pr = new Intl.PluralRules("en");
    expect(pr.select(1)).toBe("one");
  });

  test("select(2) returns 'other' for English", () => {
    const pr = new Intl.PluralRules("en");
    expect(pr.select(2)).toBe("other");
  });

  test("select returns a string", () => {
    const pr = new Intl.PluralRules("en");
    expect(typeof pr.select(5)).toBe("string");
  });
});
