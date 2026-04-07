/*---
description: Goccia.semver.SemVer.prototype.compareMain compares only major, minor, and patch
features: [Goccia.semver.SemVer]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.SemVer.prototype.compareMain", () => {
  test("compareMain ignores prerelease metadata", () => {
    const version = new Goccia.semver.SemVer("1.2.3-alpha.1");
    expect(version.compareMain("1.2.3")).toBe(0);
    expect(version.compareMain("1.2.4-alpha.1")).toBe(-1);
  });

  test("compareMain handles equal mains, build metadata, and larger version boundaries", () => {
    const version = new Goccia.semver.SemVer("1.2.3-alpha.1");
    expect(version.compareMain("1.2.3-beta.9")).toBe(0);
    expect(version.compareMain("1.2.3+build.7")).toBe(0);
    expect(version.compareMain("10.0.0")).toBe(-1);
    expect(version.compareMain("0.9.9")).toBe(1);
  });

  test("compareMain throws for invalid versions", () => {
    const version = new Goccia.semver.SemVer("1.2.3");
    expect(() => version.compareMain("bad")).toThrow(TypeError);
    expect(() => version.compareMain({})).toThrow(TypeError);
  });
});
