/*---
description: Goccia.semver exposes the SemVer 2.0.0 public API namespace
features: [Goccia, Goccia.semver]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver", () => {
  test("semver is an object", () => {
    expect(typeof Goccia.semver).toBe("object");
  });

  test("SEMVER_SPEC_VERSION is 2.0.0", () => {
    expect(Goccia.semver.SEMVER_SPEC_VERSION).toBe("2.0.0");
  });

  test("RELEASE_TYPES is an array", () => {
    expect(Array.isArray(Goccia.semver.RELEASE_TYPES)).toBe(true);
  });

  test("constructors are functions", () => {
    expect(typeof Goccia.semver.SemVer).toBe("function");
    expect(typeof Goccia.semver.Range).toBe("function");
    expect(typeof Goccia.semver.Comparator).toBe("function");
  });

  test("module groups are objects", () => {
    expect(typeof Goccia.semver.functions).toBe("object");
    expect(typeof Goccia.semver.ranges).toBe("object");
    expect(typeof Goccia.semver.classes).toBe("object");
  });

  test("module group members are functions", () => {
    expect(typeof Goccia.semver.functions.valid).toBe("function");
    expect(typeof Goccia.semver.ranges.intersects).toBe("function");
    expect(typeof Goccia.semver.classes.SemVer).toBe("function");
  });
});
