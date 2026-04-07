/*---
description: Goccia.semver.compare orders versions by semver precedence
features: [Goccia.semver.compare]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.compare", () => {
  test("Goccia.semver.compare returns -1, 0, or 1", () => {
    expect(Goccia.semver.compare("1.2.3", "1.2.4")).toBe(-1);
    expect(Goccia.semver.compare("1.2.4", "1.2.3")).toBe(1);
    expect(Goccia.semver.compare("1.2.3", "1.2.3")).toBe(0);
    expect(Goccia.semver.compare("1.2.3-alpha.1", "1.2.3")).toBe(-1);
  });

  test("Goccia.semver.compare throws for invalid versions", () => {
    expect(() => Goccia.semver.compare("1.2.3", "bad")).toThrow(TypeError);
  });
});
