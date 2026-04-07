/*---
description: Goccia.semver.SemVer.comparePre compares prerelease precedence only
features: [Goccia.semver.SemVer]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.SemVer.comparePre", () => {
  test("comparePre compares prerelease identifiers independently of main versions", () => {
    const prerelease = new Goccia.semver.SemVer("1.2.3-alpha.1");
    expect(prerelease.comparePre("1.2.3")).toBe(-1);
    expect(prerelease.comparePre("9.9.9-alpha.2")).toBe(-1);
    expect(new Goccia.semver.SemVer("1.2.3").comparePre("1.2.3-alpha.1")).toBe(1);
  });
});
