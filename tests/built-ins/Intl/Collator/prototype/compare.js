/*---
description: Intl.Collator.prototype.compare
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";

describe.runIf(isIntl)("Intl.Collator.prototype.compare", () => {
  test("compare is an accessor property on the prototype", () => {
    const desc = Object.getOwnPropertyDescriptor(Intl.Collator.prototype, "compare");
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.value).toBe(undefined);
    expect(desc.enumerable).toBe(false);
    expect(desc.configurable).toBe(true);
  });

  test("compare getter returns a cached bound function", () => {
    const collator = new Intl.Collator("en");
    const first = collator.compare;
    const second = collator.compare;

    expect(typeof first).toBe("function");
    expect(first).toBe(second);
    expect(first("a", "b") < 0).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(collator, "compare")).toBe(false);
  });

  test("compare getter creates distinct functions for distinct instances", () => {
    const first = new Intl.Collator("en").compare;
    const second = new Intl.Collator("en").compare;

    expect(first === second).toBe(false);
  });

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
