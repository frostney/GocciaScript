/*---
description: semver.satisfies matches versions against semver ranges
features: [semver.satisfies]
---*/

import * as semver from "goccia:semver";

describe("semver.satisfies", () => {
  test("semver.satisfies respects range and prerelease matching", () => {
    expect(semver.satisfies("1.5.0", "^1.2.3")).toBe(true);
    expect(semver.satisfies("2.0.0", "^1.2.3")).toBe(false);
    expect(semver.satisfies("1.2.3-alpha.1", "^1.2.3")).toBe(false);
    expect(semver.satisfies("1.2.3-alpha.2", ">=1.2.3-alpha.1 <1.2.3", {
      includePrerelease: true,
    })).toBe(true);
    expect(semver.satisfies("not-a-version", "^1.2.3")).toBe(false);
    expect(semver.satisfies("1.2.3", "invalid")).toBe(false);
  });
});
