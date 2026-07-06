/*---
description: semver.SemVer.prototype.comparePre compares prerelease precedence only
features: [semver.SemVer]
---*/

import * as semver from "goccia:semver";

describe("semver.SemVer.prototype.comparePre", () => {
  test("comparePre compares prerelease identifiers independently of main versions", () => {
    const prerelease = new semver.SemVer("1.2.3-alpha.1");
    expect(prerelease.comparePre("1.2.3")).toBe(-1);
    expect(prerelease.comparePre("9.9.9-alpha.2")).toBe(-1);
    expect(new semver.SemVer("1.2.3").comparePre("1.2.3-alpha.1")).toBe(1);
  });

  test("comparePre handles equal precedence, prerelease length, and numeric ordering", () => {
    const prerelease = new semver.SemVer("1.2.3-alpha.1");
    expect(prerelease.comparePre("7.8.9-alpha.1")).toBe(0);
    expect(new semver.SemVer("1.2.3-alpha").comparePre("1.2.3-alpha.1")).toBe(-1);
    expect(new semver.SemVer("1.2.3-alpha.2").comparePre("1.2.3-alpha.10")).toBe(-1);
  });

  test("comparePre throws for invalid versions", () => {
    expect(() => new semver.SemVer("invalid")).toThrow(TypeError);
    expect(() => new semver.SemVer("1.2.3-alpha.1").comparePre("bad")).toThrow(TypeError);
  });
});
