/*---
description: Goccia.semver.SemVer constructs semver objects with node-semver-like fields
features: [Goccia.semver.SemVer]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.SemVer", () => {
  test("new Goccia.semver.SemVer creates semver instances", () => {
    const version = new Goccia.semver.SemVer("1.2.3-alpha.1+build.5");
    expect(version instanceof Goccia.semver.SemVer).toBe(true);
    expect(version.version).toBe("1.2.3-alpha.1");
    expect(version.major).toBe(1);
    expect(version.minor).toBe(2);
    expect(version.patch).toBe(3);
    expect(version.prerelease[0]).toBe("alpha");
    expect(version.prerelease[1]).toBe(1);
    expect(version.build[0]).toBe("build");
    expect(version.build[1]).toBe("5");
  });

  test("new Goccia.semver.SemVer handles edge and invalid versions", () => {
    const minimal = new Goccia.semver.SemVer("0.0.0+meta");
    expect(minimal.version).toBe("0.0.0");
    expect(minimal.major).toBe(0);
    expect(minimal.minor).toBe(0);
    expect(minimal.patch).toBe(0);
    expect(minimal.build[0]).toBe("meta");
    expect(() => new Goccia.semver.SemVer("invalid")).toThrow(TypeError);
  });
});
