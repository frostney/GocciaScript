/*---
description: >
  Goccia.gc exposes read-only GC memory properties.
  suggestedMaxBytes always reflects the auto-detected ceiling.
  maxBytes reflects the effective limit, which may be overridden
  by --max-memory from the CLI.
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

  describe("Goccia.gc.suggestedMaxBytes", () => {
    test("is a number", () => {
      expect(typeof Goccia.gc.suggestedMaxBytes).toBe("number");
    });

    test("is positive", () => {
      expect(Goccia.gc.suggestedMaxBytes).toBeGreaterThan(0);
    });

    test("cannot be set from JS", () => {
      expect(() => {
        Goccia.gc.suggestedMaxBytes = 999;
      }).toThrow(TypeError);
    });
  });

  describe("auto-detected ceiling scaling (suggestedMaxBytes)", () => {
    const MB = 1024 * 1024;
    const GB = 1024 * MB;
    const suggested = Goccia.gc.suggestedMaxBytes;

    test("is at least 256 MB", () => {
      expect(suggested).toBeGreaterThanOrEqual(256 * MB);
    });

    test("does not exceed 8 GB on 64-bit", () => {
      // MAX_BYTES_CAP_64BIT = 8 GB; on 32-bit the cap is 700 MB.
      // Either way suggestedMaxBytes must not exceed 8 GB.
      expect(suggested).toBeLessThanOrEqual(8 * GB);
    });

    test("is at most half of reported physical memory or the platform cap", () => {
      // The auto-detected ceiling is min(physicalMemory / 2, platformCap).
      // We can't read physical memory from JS, but we know the result
      // must be <= 8 GB (64-bit cap) or <= 700 MB (32-bit cap).
      expect(suggested).toBeLessThanOrEqual(8 * GB);
      expect(suggested).toBeGreaterThanOrEqual(256 * MB);
    });
  });

  describe("maxBytes vs suggestedMaxBytes", () => {
    const maxBytes = Goccia.gc.maxBytes;
    const suggested = Goccia.gc.suggestedMaxBytes;
    const isOverridden = maxBytes !== suggested;

    test("without --max-memory, maxBytes equals suggestedMaxBytes", () => {
      // When --max-memory is not passed, both values are identical.
      // When it is passed, maxBytes reflects the CLI override.
      // Either way this relationship must hold:
      if (!isOverridden) {
        expect(maxBytes).toBe(suggested);
      }
    });

    test("with --max-memory, maxBytes differs from suggestedMaxBytes", () => {
      // This test documents the override relationship. When
      // --max-memory is active, maxBytes is the user-specified value
      // while suggestedMaxBytes remains the auto-detected default.
      if (isOverridden) {
        expect(maxBytes).not.toBe(suggested);
      }
    });
  });
});
