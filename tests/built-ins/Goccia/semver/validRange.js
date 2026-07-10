/*---
description: semver.validRange normalizes semver ranges
features: [semver.validRange]
---*/

import * as semver from "goccia:semver";

describe("semver.validRange", () => {
  test("semver.validRange returns formatted ranges or null", () => {
    expect(semver.validRange("^1.2.3")).toBe(">=1.2.3 <2.0.0-0");
    expect(semver.validRange("1.x")).toBe(">=1.0.0 <2.0.0-0");
    expect(semver.validRange("")).toBe("*");
    expect(semver.validRange("invalid")).toBe(null);
  });
});
