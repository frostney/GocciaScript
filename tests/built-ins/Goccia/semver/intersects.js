/*---
description: semver.intersects matches range intersections across OR disjuncts
features: [semver.intersects]
---*/

import * as semver from "goccia:semver";

describe("semver.intersects", () => {
  test("semver.intersects checks every comparator set pair", () => {
    expect(semver.intersects("1.x || 2.x", "2.1.x")).toBe(true);
    expect(semver.intersects("1.x", "2.x")).toBe(false);
  });
});
