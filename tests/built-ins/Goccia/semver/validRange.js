/*---
description: Goccia.semver.validRange normalizes semver ranges
features: [Goccia.semver.validRange]
---*/

test("Goccia.semver.validRange returns formatted ranges or null", () => {
  expect(Goccia.semver.validRange("^1.2.3")).toBe(">=1.2.3 <2.0.0-0");
  expect(Goccia.semver.validRange("1.x")).toBe(">=1.0.0 <2.0.0-0");
});
