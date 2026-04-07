/*---
description: Goccia.semver.SemVer.compareMain compares only major, minor, and patch
features: [Goccia.semver.SemVer]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.SemVer.compareMain", () => {
  test("compareMain ignores prerelease metadata", () => {
    const version = new Goccia.semver.SemVer("1.2.3-alpha.1");
    expect(version.compareMain("1.2.3")).toBe(0);
    expect(version.compareMain("1.2.4-alpha.1")).toBe(-1);
  });
});
