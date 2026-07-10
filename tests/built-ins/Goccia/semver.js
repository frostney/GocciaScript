/*---
description: semver exposes the SemVer 2.0.0 public API namespace
features: [Goccia, semver]
---*/

import * as semver from "goccia:semver";

describe("semver", () => {
  test("semver is an object", () => {
    expect(typeof semver).toBe("object");
  });

  test("SEMVER_SPEC_VERSION is 2.0.0", () => {
    expect(semver.SEMVER_SPEC_VERSION).toBe("2.0.0");
  });

  test("RELEASE_TYPES is an array", () => {
    expect(Array.isArray(semver.RELEASE_TYPES)).toBe(true);
  });

  test("constructors are functions", () => {
    expect(typeof semver.SemVer).toBe("function");
    expect(typeof semver.Range).toBe("function");
    expect(typeof semver.Comparator).toBe("function");
  });

  test("module groups are objects", () => {
    expect(typeof semver.functions).toBe("object");
    expect(typeof semver.ranges).toBe("object");
    expect(typeof semver.classes).toBe("object");
  });

  test("module group members are functions", () => {
    expect(typeof semver.functions.valid).toBe("function");
    expect(typeof semver.ranges.intersects).toBe("function");
    expect(typeof semver.classes.SemVer).toBe("function");
  });
});
