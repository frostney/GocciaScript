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

  test("an empty comparator reports the default options", () => {
    const comparator = new semver.Comparator("");
    expect(comparator.loose).toBe(false);
    expect(comparator.options).toEqual({
      loose: false,
      includePrerelease: false,
      rtl: false,
    });
  });

  test("an empty comparator does not keep the options of the previous one", () => {
    const all = { loose: true, includePrerelease: true, rtl: true };
    const first = new semver.Comparator("", all);
    expect(first.loose).toBe(true);
    expect(first.options).toEqual(all);

    const second = new semver.Comparator("");
    expect(second.loose).toBe(false);
    expect(second.options).toEqual({
      loose: false,
      includePrerelease: false,
      rtl: false,
    });
  });

  test("an empty comparator carries the options it was given", () => {
    const comparator = new semver.Comparator("", { loose: true });
    expect(comparator.loose).toBe(true);
    expect(comparator.options).toEqual({
      loose: true,
      includePrerelease: false,
      rtl: false,
    });
  });

  test("an operator with no version carries the options it was given", () => {
    const comparator = new semver.Comparator(">=", { loose: true });
    expect(comparator.operator).toBe("");
    expect(comparator.value).toBe("");
    expect(comparator.loose).toBe(true);
    expect(comparator.options.loose).toBe(true);
  });
});
