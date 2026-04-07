/*---
description: Goccia.semver.intersects matches range intersections across OR disjuncts
features: [Goccia.semver.intersects]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.intersects", () => {
  test("Goccia.semver.intersects checks every comparator set pair", () => {
    expect(Goccia.semver.intersects("1.x || 2.x", "2.1.x")).toBe(true);
    expect(Goccia.semver.intersects("1.x", "2.x")).toBe(false);
  });
});
