/*---
description: >
  Goccia.gc exposes read-only GC memory properties.
  When --max-memory is set from the CLI, allocations that exceed the
  limit throw a RangeError.
features: [global-properties, Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("Goccia.gc memory properties", () => {
  describe("Goccia.gc.bytesAllocated", () => {
    test("is a number", () => {
      expect(typeof Goccia.gc.bytesAllocated).toBe("number");
    });

    test("is positive", () => {
      expect(Goccia.gc.bytesAllocated).toBeGreaterThan(0);
    });

    test("cannot be set from JS", () => {
      expect(() => {
        Goccia.gc.bytesAllocated = 0;
      }).toThrow(TypeError);
    });
  });

  describe("Goccia.gc.maxBytes", () => {
    test("is a number", () => {
      expect(typeof Goccia.gc.maxBytes).toBe("number");
    });

    test("cannot be set from JS", () => {
      expect(() => {
        Goccia.gc.maxBytes = 999;
      }).toThrow(TypeError);
    });
  });
});
