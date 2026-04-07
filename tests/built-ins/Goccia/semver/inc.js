/*---
description: Goccia.semver.inc increments versions by release type
features: [Goccia.semver.inc]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.inc", () => {
  test("Goccia.semver.inc increments patch and prerelease versions", () => {
    expect(Goccia.semver.inc("1.2.3", "patch")).toBe("1.2.4");
    expect(Goccia.semver.inc("1.2.3", "prerelease", "beta")).toBe("1.2.4-beta.0");
    expect(Goccia.semver.inc("1.2.4-beta.0", "prerelease", "beta")).toBe("1.2.4-beta.1");
  });

  test("Goccia.semver.inc returns null for invalid input", () => {
    expect(Goccia.semver.inc("1.2.3", "foobar")).toBe(null);
    expect(Goccia.semver.inc("1.2", "patch")).toBe(null);
  });
});
