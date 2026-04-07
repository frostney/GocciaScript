/*---
description: Goccia.semver.Comparator constructs comparator objects
features: [Goccia.semver.Comparator]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.Comparator", () => {
  test("new Goccia.semver.Comparator builds comparator instances", () => {
    const comparator = new Goccia.semver.Comparator(">=1.2.3");
    expect(comparator instanceof Goccia.semver.Comparator).toBe(true);
    expect(comparator.operator).toBe(">=");
    expect(comparator.value).toBe(">=1.2.3");
    expect(comparator.test("1.2.3")).toBe(true);
    expect(comparator.test("1.2.2")).toBe(false);
  });

  test("new Goccia.semver.Comparator handles malformed and empty comparators", () => {
    expect(() => new Goccia.semver.Comparator(">>1.2.3")).toThrow(TypeError);
    const comparator = new Goccia.semver.Comparator("");
    expect(comparator.operator).toBe("");
    expect(comparator.value).toBe("");
  });
});
