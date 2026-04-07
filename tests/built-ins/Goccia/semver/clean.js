/*---
description: Goccia.semver.clean trims and normalizes valid versions
features: [Goccia.semver.clean]
---*/

test("Goccia.semver.clean returns cleaned versions or null", () => {
  expect(Goccia.semver.clean("  =v1.2.3  ")).toBe("1.2.3");
  expect(Goccia.semver.clean("~1.0.0")).toBe(null);
});
