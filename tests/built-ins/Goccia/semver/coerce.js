/*---
description: Goccia.semver.coerce extracts the first semver-like version tuple
features: [Goccia.semver.coerce]
---*/

test("Goccia.semver.coerce returns a SemVer object or null", () => {
  const version = Goccia.semver.coerce("v3.4 replaces v3.3.1");
  expect(version instanceof Goccia.semver.SemVer).toBe(true);
  expect(version.version).toBe("3.4.0");
  expect(Goccia.semver.coerce("version one")).toBe(null);
});
