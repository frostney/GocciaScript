/*---
description: >
  A call's arguments are copied into the callee's registers, and the caller's
  copies are dead from then on. When the callee drops its own reference to an
  argument it received, a collection inside the callee reclaims the value: the
  caller's argument registers no longer keep it reachable (ES2026 §10.2.1
  [[Call]], §10.2.1.1 PrepareForOrdinaryCall).
features: [functions, arrow-functions, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

const collect = () => {
  Goccia.gc();
  return Goccia.gc.bytesAllocated;
};

describe("an argument the callee has dropped", () => {
  test("is reclaimed when a plain call passes a fresh value", () => {
    const baseline = collect();
    const f = (buffer) => {
      const length = buffer.byteLength;
      buffer = null;
      Goccia.gc();
      return [length, Goccia.gc.bytesAllocated - baseline];
    };
    const [length, grown] = f(new ArrayBuffer(SIZE));
    expect(length).toBe(SIZE);
    expect(grown).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed when a method call passes it", () => {
    const baseline = collect();
    const target = {
      take(buffer) {
        buffer = null;
        Goccia.gc();
        return Goccia.gc.bytesAllocated - baseline;
      },
    };
    expect(target.take(new ArrayBuffer(SIZE))).toBeLessThan(SIZE / 2);
  });

  test("is reclaimed for each of several object arguments", () => {
    const baseline = collect();
    const f = (first, second, third) => {
      const total = first.byteLength + second.byteLength + third.byteLength;
      first = null;
      second = null;
      third = null;
      Goccia.gc();
      return [total, Goccia.gc.bytesAllocated - baseline];
    };
    const [total, grown] = f(
      new ArrayBuffer(SIZE / 4),
      new ArrayBuffer(SIZE / 4),
      new ArrayBuffer(SIZE / 4),
    );
    expect(total).toBe((SIZE / 4) * 3);
    expect(grown).toBeLessThan(SIZE / 8);
  });
});

describe("an argument a callee drops through a for-of head", () => {
  test("is reclaimed when the head assigns the parameter", () => {
    const baseline = collect();
    const f = (buffer) => {
      for (buffer of [0]) {
        // The head wrote the parameter.
      }
      Goccia.gc();
      return [buffer, Goccia.gc.bytesAllocated - baseline];
    };
    const [value, grown] = f(new ArrayBuffer(SIZE));
    expect(value).toBe(0);
    expect(grown).toBeLessThan(SIZE / 2);
  });
});

describe("a callee that still holds its arguments", () => {
  test("keeps the value while the parameter is live", () => {
    const f = (buffer, label) => {
      Goccia.gc();
      return [buffer.byteLength, label];
    };
    expect(f(new ArrayBuffer(1024), "kept")).toEqual([1024, "kept"]);
  });

  test("sees every argument through a rest parameter", () => {
    const f = (...rest) => {
      rest = rest.map((item) => item.byteLength);
      Goccia.gc();
      return rest;
    };
    expect(f(new ArrayBuffer(8), new ArrayBuffer(16), new ArrayBuffer(24))).toEqual([8, 16, 24]);
  });

  test("sees a default parameter that reads an earlier argument", () => {
    const f = (buffer, length = buffer.byteLength) => {
      buffer = null;
      Goccia.gc();
      return length;
    };
    expect(f(new ArrayBuffer(2048))).toBe(2048);
  });

  test("returns the value when the callee hands its parameter back", () => {
    const keep = (value) => {
      const result = value;
      value = null;
      Goccia.gc();
      return result;
    };
    const buffer = new ArrayBuffer(4096);
    expect(keep(buffer)).toBe(buffer);
  });

  test("a caller that reuses a variable it passed sees it unchanged", () => {
    const f = (value) => {
      value = null;
      return value;
    };
    const original = { id: 1 };
    expect(f(original)).toBe(null);
    expect(original.id).toBe(1);
    expect(f(original)).toBe(null);
    expect(original).toEqual({ id: 1 });
  });
});
