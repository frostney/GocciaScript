/*---
description: Goccia.version exposes the engine semver version string
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.version", () => {
  test("version is a string", () => {
    expect(typeof Goccia.version).toBe("string");
  });

  test("version starts with a semver base", () => {
    const base = Goccia.version.split("-")[0];
    const parts = base.split(".");
    expect(parts.length).toBe(3);
    expect(Number.isNaN(Number(parts[0]))).toBe(false);
    expect(Number.isNaN(Number(parts[1]))).toBe(false);
    expect(Number.isNaN(Number(parts[2]))).toBe(false);
  });

  test("version is either a release or a dev build", () => {
    const version = Goccia.version;
    const isRelease = version.split("-").length === 1;
    const isDev = version.endsWith("-dev");
    expect(isRelease || isDev).toBe(true);
  });
});
