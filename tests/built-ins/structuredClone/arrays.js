/*---
description: structuredClone deep-clones arrays
features: [structuredClone]
---*/

test("clones a simple array", () => {
  const original = [1, 2, 3];
  const clone = structuredClone(original);
  expect(clone.length).toBe(3);
  expect(clone[0]).toBe(1);
  expect(clone[1]).toBe(2);
  expect(clone[2]).toBe(3);
});

test("clone is a distinct array", () => {
  const original = [1, 2, 3];
  const clone = structuredClone(original);
  clone[0] = 99;
  expect(original[0]).toBe(1);
  expect(clone[0]).toBe(99);
});

test("clones nested arrays", () => {
  const original = [[1, 2], [3, [4, 5]]];
  const clone = structuredClone(original);
  expect(clone[0][0]).toBe(1);
  expect(clone[1][1][0]).toBe(4);
  clone[0][0] = 99;
  expect(original[0][0]).toBe(1);
});

test("clones array with objects", () => {
  const original = [{ a: 1 }, { b: 2 }];
  const clone = structuredClone(original);
  expect(clone[0].a).toBe(1);
  expect(clone[1].b).toBe(2);
  clone[0].a = 99;
  expect(original[0].a).toBe(1);
});

test("clones empty array", () => {
  const clone = structuredClone([]);
  expect(clone.length).toBe(0);
});
// StructuredSerialize enumerates with EnumerableOwnPropertyNames
// (ECMA-262 §7.3.23), which takes each value with Get, and StructuredDeserialize
// reinstalls it with CreateDataProperty. An accessor is therefore never
// preserved: its getter runs and the RESULT is what gets cloned, as a plain data
// property. Oracles: node 24 and bun 1.3 agree on every case below.
describe("accessor and non-element properties", () => {
  test("serializes the value an accessor index getter returns", () => {
    const original = [1, 2, 3];
    Object.defineProperty(original, 1, {
      get: () => 42,
      enumerable: true,
      configurable: true,
    });

    const clone = structuredClone(original);
    expect(clone.length).toBe(3);
    expect(clone[0]).toBe(1);
    expect(clone[1]).toBe(42);
    expect(clone[2]).toBe(3);
  });

  test("clones an accessor index as a plain data property", () => {
    const original = [1];
    Object.defineProperty(original, 0, {
      get: () => 7,
      enumerable: true,
      configurable: false,
    });

    const descriptor = Object.getOwnPropertyDescriptor(
      structuredClone(original),
      "0",
    );
    expect(descriptor).toEqual({
      value: 7,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("runs an accessor index getter exactly once", () => {
    let reads = 0;
    const original = [1, 2];
    Object.defineProperty(original, 0, {
      get: () => {
        reads += 1;
        return 7;
      },
      enumerable: true,
      configurable: true,
    });

    expect(structuredClone(original)[0]).toBe(7);
    expect(reads).toBe(1);
  });

  test("lets a throwing accessor index getter propagate", () => {
    const original = [1, 2];
    Object.defineProperty(original, 0, {
      get: () => {
        throw new Error("index-getter-boom");
      },
      enumerable: true,
      configurable: true,
    });

    expect(() => structuredClone(original)).toThrow("index-getter-boom");
  });

  test("rejects an accessor index getter returning an unclonable value", () => {
    const original = [1];
    Object.defineProperty(original, 0, {
      get: () => () => {},
      enumerable: true,
      configurable: true,
    });

    expect(() => structuredClone(original)).toThrow(DOMException);
  });

  test("keeps an accessor index past the end and the holes before it", () => {
    const original = [1];
    Object.defineProperty(original, 3, {
      get: () => 3,
      enumerable: true,
      configurable: true,
    });

    const clone = structuredClone(original);
    expect(clone.length).toBe(4);
    expect(clone[3]).toBe(3);
    expect(Object.prototype.hasOwnProperty.call(clone, "2")).toBe(false);
  });

  test("skips a non-enumerable index", () => {
    const original = [1, 2, 3];
    Object.defineProperty(original, 1, {
      get: () => 42,
      enumerable: false,
      configurable: true,
    });

    const clone = structuredClone(original);
    expect(clone.length).toBe(3);
    expect(Object.prototype.hasOwnProperty.call(clone, "1")).toBe(false);
  });

  test("clones non-index own properties", () => {
    const original = [1];
    original.extra = "e";
    Object.defineProperty(original, "computed", {
      get: () => "c",
      enumerable: true,
      configurable: true,
    });

    const clone = structuredClone(original);
    expect(Array.isArray(clone)).toBe(true);
    expect(clone.extra).toBe("e");
    expect(clone.computed).toBe("c");
    expect(Object.keys(clone)).toEqual(["0", "extra", "computed"]);
  });

  test("preserves holes", () => {
    const clone = structuredClone([1, , 3]);
    expect(clone.length).toBe(3);
    expect(Object.prototype.hasOwnProperty.call(clone, "1")).toBe(false);
  });

  test("snapshots the length before any getter runs", () => {
    const original = [1, 2, 3];
    Object.defineProperty(original, 0, {
      get: () => {
        original.length = 1;
        return 7;
      },
      enumerable: true,
      configurable: true,
    });

    const clone = structuredClone(original);
    expect(clone.length).toBe(3);
    expect(clone[0]).toBe(7);
    expect(Object.prototype.hasOwnProperty.call(clone, "1")).toBe(false);
  });

  test("does not carry a frozen array's restrictions to the clone", () => {
    const clone = structuredClone(Object.freeze([1, 2]));
    expect(Object.isFrozen(clone)).toBe(false);
    expect(Object.getOwnPropertyDescriptor(clone, "0")).toEqual({
      value: 1,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });
});

test("structuredClone rejects excessive acyclic nesting", () => {
  const value = Array(257).fill(0).reduce((child) => [child], 0);
  expect(() => structuredClone(value)).toThrow(RangeError);
});
