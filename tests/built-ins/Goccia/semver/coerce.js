/*---
description: semver.coerce extracts the first semver-like version tuple
features: [semver.coerce]
---*/

import * as semver from "goccia:semver";

describe("semver.coerce", () => {
  test("semver.coerce returns a SemVer object or null", () => {
    const version = semver.coerce("v3.4 replaces v3.3.1");
    expect(version instanceof semver.SemVer).toBe(true);
    expect(version.version).toBe("3.4.0");
    expect(semver.coerce("version one")).toBe(null);
  });
});
