/*---
description: semver.Range constructs range objects
features: [semver.Range]
---*/

import * as semver from "goccia:semver";

describe("semver.Range", () => {
  test("new semver.Range builds range instances", () => {
    const range = new semver.Range("^1.2.3");
    expect(range instanceof semver.Range).toBe(true);
    expect(range.range).toBe(">=1.2.3 <2.0.0-0");
    expect(range.test("1.5.0")).toBe(true);
    expect(range.test("2.0.0")).toBe(false);
  });

  test("the any comparator of a wildcard range carries the range's options", () => {
    const options = { loose: true, includePrerelease: true };
    const comparator = new semver.Range("*", options).set[0][0];
    expect(comparator.value).toBe("");
    expect(comparator.loose).toBe(true);
    expect(comparator.options).toEqual({
      loose: true,
      includePrerelease: true,
      rtl: false,
    });
  });

  test("the any comparator of an empty range carries the default options", () => {
    const comparator = new semver.Range("").set[0][0];
    expect(comparator.value).toBe("");
    expect(comparator.loose).toBe(false);
    expect(comparator.options).toEqual({
      loose: false,
      includePrerelease: false,
      rtl: false,
    });
  });
});
