/*---
description: Goccia global exposes the semver public API namespace
features: [global-properties, Goccia, Goccia.semver]
---*/

test("Goccia global exposes semver namespace", () => {
  expect(typeof Goccia).toBe("object");
  expect(typeof Goccia.semver).toBe("object");
  expect(Goccia.semver.SEMVER_SPEC_VERSION).toBe("2.0.0");
  expect(Array.isArray(Goccia.semver.RELEASE_TYPES)).toBe(true);
  expect(typeof Goccia.semver.SemVer).toBe("function");
  expect(typeof Goccia.semver.Range).toBe("function");
  expect(typeof Goccia.semver.Comparator).toBe("function");
  expect(typeof Goccia.semver.functions).toBe("object");
  expect(typeof Goccia.semver.ranges).toBe("object");
  expect(typeof Goccia.semver.classes).toBe("object");
});
