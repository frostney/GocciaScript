/*---
description: semver.SemVer constructs semver objects with node-semver-like fields
features: [semver.SemVer]
---*/

import * as semver from "goccia:semver";

describe("semver.SemVer", () => {
  test("new semver.SemVer creates semver instances", () => {
    const version = new semver.SemVer("1.2.3-alpha.1+build.5");
    expect(version instanceof semver.SemVer).toBe(true);
    expect(version.version).toBe("1.2.3-alpha.1");
    expect(version.major).toBe(1);
    expect(version.minor).toBe(2);
    expect(version.patch).toBe(3);
    expect(version.prerelease[0]).toBe("alpha");
    expect(version.prerelease[1]).toBe(1);
    expect(version.build[0]).toBe("build");
    expect(version.build[1]).toBe("5");
  });

  test("new semver.SemVer handles edge and invalid versions", () => {
    const minimal = new semver.SemVer("0.0.0+meta");
    expect(minimal.version).toBe("0.0.0");
    expect(minimal.major).toBe(0);
    expect(minimal.minor).toBe(0);
    expect(minimal.patch).toBe(0);
    expect(minimal.build[0]).toBe("meta");
    expect(() => new semver.SemVer("invalid")).toThrow(TypeError);
  });
});
