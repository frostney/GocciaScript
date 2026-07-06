/*---
description: semver.clean trims and normalizes valid versions
features: [semver.clean]
---*/

import * as semver from "goccia:semver";

describe("semver.clean", () => {
  test("semver.clean returns cleaned versions or null", () => {
    expect(semver.clean("  =v1.2.3  ")).toBe("1.2.3");
    expect(semver.clean("~1.0.0")).toBe(null);
  });
});
