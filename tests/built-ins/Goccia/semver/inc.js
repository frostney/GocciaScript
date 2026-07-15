/*---
description: semver.inc increments versions by release type
features: [semver.inc]
---*/

import * as semver from "goccia:semver";

describe("semver.inc", () => {
  test("semver.inc increments patch and prerelease versions", () => {
    expect(semver.inc("1.2.3", "patch")).toBe("1.2.4");
    expect(semver.inc("1.2.3", "prerelease", "beta")).toBe("1.2.4-beta.0");
    expect(semver.inc("1.2.4-beta.0", "prerelease", "beta")).toBe("1.2.4-beta.1");
  });

  test("semver.inc returns null for invalid input", () => {
    expect(semver.inc("1.2.3", "foobar")).toBe(null);
    expect(semver.inc("1.2", "patch")).toBe(null);
  });
});
