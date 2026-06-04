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

    test("gc preserves reachable objects after expression temporaries", () => {
      const array = [];
      array.length;
      Goccia.gc();
      array.t = 3;
      Goccia.gc();
      expect(array.t).toBe(3);
    });

    test("gc preserves nested recursive binary expression temporaries", () => {
      const fib = (n) => {
        Goccia.gc();
        return n < 2 ? n : fib(n - 1) + fib(n - 2);
      };

      expect(fib(8)).toBe(21);
    });
  });
});
