/*---
description: semver.toComparators expands ranges into comparator sets
features: [semver.toComparators]
---*/

import * as semver from "goccia:semver";

describe("semver.toComparators", () => {
  test("semver.toComparators expands shorthand ranges", () => {
    const comparators = semver.toComparators("1.x");
    expect(Array.isArray(comparators)).toBe(true);
    expect(comparators[0][0]).toBe(">=1.0.0");
    expect(comparators[0][1]).toBe("<2.0.0-0");
  });

  test("semver.toComparators handles partial and invalid ranges", () => {
    const comparators = semver.toComparators("1.2");
    expect(comparators[0][0]).toBe(">=1.2.0");
    expect(comparators[0][1]).toBe("<1.3.0-0");
    expect(() => semver.toComparators("invalid")).toThrow();
  });
});
