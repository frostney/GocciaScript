/*---
description: semver.Comparator constructs comparator objects
features: [semver.Comparator]
---*/

import * as semver from "goccia:semver";

describe("semver.Comparator", () => {
  test("new semver.Comparator builds comparator instances", () => {
    const comparator = new semver.Comparator(">=1.2.3");
    expect(comparator instanceof semver.Comparator).toBe(true);
    expect(comparator.operator).toBe(">=");
    expect(comparator.value).toBe(">=1.2.3");
    expect(comparator.test("1.2.3")).toBe(true);
    expect(comparator.test("1.2.2")).toBe(false);
  });

  test("new semver.Comparator handles malformed and empty comparators", () => {
    expect(() => new semver.Comparator(">>1.2.3")).toThrow(TypeError);
    const comparator = new semver.Comparator("");
    expect(comparator.operator).toBe("");
    expect(comparator.value).toBe("");
  });
});
