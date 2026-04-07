/*---
description: Goccia.semver.SemVer constructs semver objects with node-semver-like fields
features: [Goccia.semver.SemVer]
---*/

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
