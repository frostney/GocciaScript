/*---
description: Intl.Collator.prototype.compare
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.Collator.prototype.compare", () => {
  test("returns negative when first string sorts before second", () => {
    const collator = new Intl.Collator("en");
    expect(collator.compare("a", "b") < 0).toBe(true);
  });

  test("returns zero for identical strings", () => {
    const collator = new Intl.Collator("en");
    expect(collator.compare("a", "a")).toBe(0);
  });

  test("returns positive when first string sorts after second", () => {
    const collator = new Intl.Collator("en");
    expect(collator.compare("b", "a") > 0).toBe(true);
  });

  test("can be used as an Array.prototype.sort comparator", () => {
    const collator = new Intl.Collator("en");
    const sorted = ["banana", "apple", "cherry"].sort(collator.compare);
    expect(sorted[0]).toBe("apple");
    expect(sorted[1]).toBe("banana");
    expect(sorted[2]).toBe("cherry");
  });
});
