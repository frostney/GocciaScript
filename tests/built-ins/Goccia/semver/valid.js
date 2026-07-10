/*---
description: semver.valid validates strict semver strings
features: [semver.valid]
---*/

import * as semver from "goccia:semver";

describe("semver.valid", () => {
  test("semver.valid returns normalized versions or null", () => {
    expect(semver.valid("1.2.3")).toBe("1.2.3");
    expect(semver.valid("v1.2.3")).toBe("1.2.3");
    expect(semver.valid("1.2")).toBe(null);
    expect(semver.valid("1.2.03")).toBe(null);
  });
});
