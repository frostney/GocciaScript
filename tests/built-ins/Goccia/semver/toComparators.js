/*---
description: Goccia.semver.toComparators expands ranges into comparator sets
features: [Goccia.semver.toComparators]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.toComparators", () => {
  test("Goccia.semver.toComparators expands shorthand ranges", () => {
    const comparators = Goccia.semver.toComparators("1.x");
    expect(Array.isArray(comparators)).toBe(true);
    expect(comparators[0][0]).toBe(">=1.0.0");
    expect(comparators[0][1]).toBe("<2.0.0-0");
  });

  test("Goccia.semver.toComparators handles partial and invalid ranges", () => {
    const comparators = Goccia.semver.toComparators("1.2");
    expect(comparators[0][0]).toBe(">=1.2.0");
    expect(comparators[0][1]).toBe("<1.3.0-0");
    expect(() => Goccia.semver.toComparators("invalid")).toThrow();
  });
});
