/*---
description: Goccia.semver.Range constructs range objects
features: [Goccia.semver.Range]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.semver.Range", () => {
  test("new Goccia.semver.Range builds range instances", () => {
    const range = new Goccia.semver.Range("^1.2.3");
    expect(range instanceof Goccia.semver.Range).toBe(true);
    expect(range.range).toBe(">=1.2.3 <2.0.0-0");
    expect(range.test("1.5.0")).toBe(true);
    expect(range.test("2.0.0")).toBe(false);
  });
});
