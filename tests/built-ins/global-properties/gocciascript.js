/*---
description: >
  GocciaScript is a global object that exposes engine metadata:
  version string and an array of enabled built-in module flags.
features: [global-properties, GocciaScript]
---*/

const isGocciaScript = typeof GocciaScript !== "undefined";

describe.runIf(isGocciaScript)("GocciaScript global", () => {
  test("GocciaScript is an object", () => {
    expect(typeof GocciaScript).toBe("object");
  });

  test("GocciaScript cannot be reassigned", () => {
    expect(() => {
      GocciaScript = {};
    }).toThrow(TypeError);
  });
});

describe.runIf(isGocciaScript)("GocciaScript.version", () => {
  test("version is a string", () => {
    expect(typeof GocciaScript.version).toBe("string");
  });

  test("version starts with a semver base", () => {
    const base = GocciaScript.version.split("-")[0];
    const parts = base.split(".");
    expect(parts.length).toBe(3);
    expect(Number.isNaN(Number(parts[0]))).toBe(false);
    expect(Number.isNaN(Number(parts[1]))).toBe(false);
    expect(Number.isNaN(Number(parts[2]))).toBe(false);
  });

  test("version is either a release or a dev build", () => {
    const version = GocciaScript.version;
    const isRelease = version.split("-").length === 1;
    const isDev = version.endsWith("-dev");
    expect(isRelease || isDev).toBe(true);
  });
});

describe.runIf(isGocciaScript)("GocciaScript.commit", () => {
  test("commit is a string", () => {
    expect(typeof GocciaScript.commit).toBe("string");
  });

  test("commit is a short hex hash", () => {
    expect(GocciaScript.commit.length > 0).toBe(true);
    expect(GocciaScript.commit.length <= 12).toBe(true);
  });
});

describe.runIf(isGocciaScript)("GocciaScript.builtIns", () => {
  test("builtIns is an array", () => {
    expect(Array.isArray(GocciaScript.builtIns)).toBe(true);
  });

  test("builtIns is not empty", () => {
    expect(GocciaScript.builtIns.length > 0).toBe(true);
  });

  test("builtIns contains only strings", () => {
    GocciaScript.builtIns.forEach((name) => {
      expect(typeof name).toBe("string");
    });
  });
});
