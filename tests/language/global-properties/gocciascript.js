/*---
description: >
  GocciaScript is a global object that exposes engine metadata:
  version string and an array of enabled built-in module flags.
features: [global-properties, GocciaScript]
---*/

describe("GocciaScript global", () => {
  test("GocciaScript is an object", () => {
    expect(typeof GocciaScript).toBe("object");
  });

  test("GocciaScript cannot be reassigned", () => {
    expect(() => {
      GocciaScript = {};
    }).toThrow();
  });
});

describe("GocciaScript.version", () => {
  test("version is a string", () => {
    expect(typeof GocciaScript.version).toBe("string");
  });

  test("version follows semver format", () => {
    const parts = GocciaScript.version.split(".");
    expect(parts.length).toBe(3);
    expect(Number.isNaN(Number(parts[0]))).toBe(false);
    expect(Number.isNaN(Number(parts[1]))).toBe(false);
    expect(Number.isNaN(Number(parts[2]))).toBe(false);
  });
});

describe("GocciaScript.builtIns", () => {
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
