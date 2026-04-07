/*---
description: >
  Goccia is a global object that exposes engine metadata (version, commit,
  builtIns, strictTypes) and the semver public API namespace.
features: [global-properties, Goccia, Goccia.semver]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia global", () => {
  test("Goccia is an object", () => {
    expect(typeof Goccia).toBe("object");
  });

  test("Goccia cannot be reassigned", () => {
    expect(() => {
      Goccia = {};
    }).toThrow(TypeError);
  });
});

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

describe.runIf(hasGoccia)("Goccia.commit", () => {
  test("commit is a string", () => {
    expect(typeof Goccia.commit).toBe("string");
  });

  test("commit is a short hex hash", () => {
    expect(Goccia.commit.length > 0).toBe(true);
    expect(Goccia.commit.length <= 12).toBe(true);
  });
});

describe.runIf(hasGoccia)("Goccia.builtIns", () => {
  test("builtIns is an array", () => {
    expect(Array.isArray(Goccia.builtIns)).toBe(true);
  });

  test("builtIns is not empty", () => {
    expect(Goccia.builtIns.length > 0).toBe(true);
  });

  test("builtIns contains only strings", () => {
    Goccia.builtIns.forEach((name) => {
      expect(typeof name).toBe("string");
    });
  });
});

describe.runIf(hasGoccia)("Goccia.semver", () => {
  test("Goccia global exposes semver namespace", () => {
    expect(typeof Goccia.semver).toBe("object");
    expect(Goccia.semver.SEMVER_SPEC_VERSION).toBe("2.0.0");
    expect(Array.isArray(Goccia.semver.RELEASE_TYPES)).toBe(true);
    expect(typeof Goccia.semver.SemVer).toBe("function");
    expect(typeof Goccia.semver.Range).toBe("function");
    expect(typeof Goccia.semver.Comparator).toBe("function");
    expect(typeof Goccia.semver.functions).toBe("object");
    expect(typeof Goccia.semver.ranges).toBe("object");
    expect(typeof Goccia.semver.classes).toBe("object");
    expect(typeof Goccia.semver.functions.valid).toBe("function");
    expect(typeof Goccia.semver.ranges.intersects).toBe("function");
    expect(typeof Goccia.semver.classes.SemVer).toBe("function");
  });
});
