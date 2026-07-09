/*---
description: semver.Range constructs range objects
features: [semver.Range]
---*/

import * as semver from "goccia:semver";

describe("semver.Range", () => {
  test("new semver.Range builds range instances", () => {
    const range = new semver.Range("^1.2.3");
    expect(range instanceof semver.Range).toBe(true);
    expect(range.range).toBe(">=1.2.3 <2.0.0-0");
    expect(range.test("1.5.0")).toBe(true);
    expect(range.test("2.0.0")).toBe(false);
  });
});
