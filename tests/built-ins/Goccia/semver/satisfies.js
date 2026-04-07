/*---
description: Goccia.semver.satisfies matches versions against semver ranges
features: [Goccia.semver.satisfies]
---*/

test("Goccia.semver.satisfies respects range and prerelease matching", () => {
  expect(Goccia.semver.satisfies("1.5.0", "^1.2.3")).toBe(true);
  expect(Goccia.semver.satisfies("2.0.0", "^1.2.3")).toBe(false);
  expect(Goccia.semver.satisfies("1.2.3-alpha.1", "^1.2.3")).toBe(false);
  expect(Goccia.semver.satisfies("1.2.3-alpha.2", ">=1.2.3-alpha.1 <1.2.3", {
    includePrerelease: true,
  })).toBe(true);
});
