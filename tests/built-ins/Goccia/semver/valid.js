/*---
description: Goccia.semver.valid validates strict semver strings
features: [Goccia.semver.valid]
---*/

test("Goccia.semver.valid returns normalized versions or null", () => {
  expect(Goccia.semver.valid("1.2.3")).toBe("1.2.3");
  expect(Goccia.semver.valid("v1.2.3")).toBe("1.2.3");
  expect(Goccia.semver.valid("1.2")).toBe(null);
  expect(Goccia.semver.valid("1.2.03")).toBe(null);
});
