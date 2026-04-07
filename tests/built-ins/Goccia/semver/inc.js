/*---
description: Goccia.semver.inc increments versions by release type
features: [Goccia.semver.inc]
---*/

test("Goccia.semver.inc increments patch and prerelease versions", () => {
  expect(Goccia.semver.inc("1.2.3", "patch")).toBe("1.2.4");
  expect(Goccia.semver.inc("1.2.3", "prerelease", "beta")).toBe("1.2.4-beta.0");
  expect(Goccia.semver.inc("1.2.4-beta.0", "prerelease", "beta")).toBe("1.2.4-beta.1");
});
