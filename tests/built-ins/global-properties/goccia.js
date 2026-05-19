/*---
description: >
  Goccia is a const global object that exposes engine metadata and utility APIs.
features: [global-properties, Goccia]
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

  describe("Goccia.gc", () => {
    test("gc is a function", () => {
      expect(typeof Goccia.gc).toBe("function");
    });

    test("gc returns undefined", () => {
      const result = Goccia.gc();
      expect(result).toBe(undefined);
    });

    test("gc does not interrupt test result aggregation", () => {
      Goccia.gc();
      expect(1).toBe(1);
    });

    test("gc can be called multiple times", () => {
      Goccia.gc();
      Goccia.gc();
    });
  });
});
