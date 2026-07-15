/*---
description: semver.compare orders versions by semver precedence
features: [semver.compare]
---*/

import * as semver from "goccia:semver";

describe("semver.compare", () => {
  test("semver.compare returns -1, 0, or 1", () => {
    expect(semver.compare("1.2.3", "1.2.4")).toBe(-1);
    expect(semver.compare("1.2.4", "1.2.3")).toBe(1);
    expect(semver.compare("1.2.3", "1.2.3")).toBe(0);
    expect(semver.compare("1.2.3-alpha.1", "1.2.3")).toBe(-1);
  });

  test("semver.compare throws for invalid versions", () => {
    expect(() => semver.compare("1.2.3", "bad")).toThrow(TypeError);
  });
});
