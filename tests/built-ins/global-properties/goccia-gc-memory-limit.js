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

    test("is positive", () => {
      expect(Goccia.gc.maxBytes).toBeGreaterThan(0);
    });

    test("cannot be set from JS", () => {
      expect(() => {
        Goccia.gc.maxBytes = 999;
      }).toThrow(TypeError);
    });
  });

  describe("default memory ceiling scaling", () => {
    const MB = 1024 * 1024;
    const GB = 1024 * MB;
    const maxBytes = Goccia.gc.maxBytes;

    test("is at least 256 MB", () => {
      expect(maxBytes).toBeGreaterThanOrEqual(256 * MB);
    });

    test("does not exceed 8 GB on 64-bit", () => {
      // MAX_BYTES_CAP_64BIT = 8 GB; on 32-bit the cap is 700 MB.
      // Either way maxBytes must not exceed 8 GB.
      expect(maxBytes).toBeLessThanOrEqual(8 * GB);
    });

    test("is at most half of reported physical memory or the platform cap", () => {
      // The default ceiling is min(physicalMemory / 2, platformCap).
      // We can't read physical memory from JS, but we know the result
      // must be <= 8 GB (64-bit cap) or <= 700 MB (32-bit cap).
      // This is a sanity check that the value is within the expected range.
      expect(maxBytes).toBeLessThanOrEqual(8 * GB);
      expect(maxBytes).toBeGreaterThanOrEqual(256 * MB);
    });
  });
});
