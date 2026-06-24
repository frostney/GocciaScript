/*---
description: Intl.Collator.prototype.compare
features: [Intl]
---*/

const isIntl = typeof Intl !== "undefined";
const hasFullICU = isIntl && new Intl.NumberFormat("en-US").format(NaN) === "NaN";

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

  test("base sensitivity ignores case and accents", () => {
    const collator = new Intl.Collator("en", { sensitivity: "base" });
    if (!hasFullICU) {
      expect(collator.compare("a", "A") === 0).toBe(false);
      return;
    }
    expect(collator.compare("a", "A")).toBe(0);
    expect(collator.compare("a", "\u00e1")).toBe(0);
  });

  test("accent sensitivity ignores case but distinguishes accents", () => {
    const collator = new Intl.Collator("en", { sensitivity: "accent" });
    if (!hasFullICU) {
      expect(collator.compare("a", "A") === 0).toBe(false);
      return;
    }
    expect(collator.compare("a", "A")).toBe(0);
    expect(collator.compare("a", "\u00e1") === 0).toBe(false);
  });

  test("case sensitivity ignores accents but distinguishes case", () => {
    const collator = new Intl.Collator("en", { sensitivity: "case" });
    if (!hasFullICU) {
      expect(typeof collator.compare("a", "\u00e1")).toBe("number");
      return;
    }
    expect(collator.compare("a", "\u00e1")).toBe(0);
    expect(collator.compare("a", "A") === 0).toBe(false);
  });

  test("numeric collation compares decimal digit sequences by numeric value", () => {
    const collator = new Intl.Collator("en", { numeric: true });
    if (!hasFullICU) {
      expect(collator.compare("2", "10") > 0).toBe(true);
      return;
    }
    expect(collator.compare("2", "10") < 0).toBe(true);
  });

  test("numeric Unicode extension compares decimal digit sequences by numeric value", () => {
    const collator = new Intl.Collator("en-u-kn-true");
    if (!hasFullICU) {
      expect(collator.compare("2", "10") > 0).toBe(true);
      return;
    }
    expect(collator.compare("2", "10") < 0).toBe(true);
  });

  test("ignored Unicode extension values do not affect comparison", () => {
    const values = ["\u212b", "\u00c5", "A\u030a", "hello"];
    const defaultCollator = new Intl.Collator();
    const locale = defaultCollator.resolvedOptions().locale;
    const ignoredExtensionCollator = new Intl.Collator(locale + "-u-co-search");

    expect(values.slice().sort(ignoredExtensionCollator.compare).join("|")).toBe(
      values.slice().sort(defaultCollator.compare).join("|"),
    );
  });
});
