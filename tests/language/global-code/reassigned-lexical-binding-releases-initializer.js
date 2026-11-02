/*---
description: >
  A script's top-level let binding, and a let binding pattern, hold their
  value in the global binding alone (ES2026 §9.1.1.4 Global Environment
  Records): once the script assigns the binding again, the initializer is
  unreachable and a collection reclaims it, also when a closure compiled before
  the declaration names the binding.
features: [let, destructuring, closures, Goccia.gc]
---*/

const SIZE = 32 * 1024 * 1024;

// Every shape runs at the script's top level, where the bindings are global
// bindings, so each measurement is taken here and the tests below assert on
// the recorded numbers.
const measure = (drop) => {
  Goccia.gc();
  const before = Goccia.gc.bytesAllocated;
  drop();
  Goccia.gc();
  return before - Goccia.gc.bytesAllocated;
};

let plain = new ArrayBuffer(SIZE);
const plainFreed = measure(() => {
  plain = null;
});

let [arrayElement] = [new ArrayBuffer(SIZE)];
const arrayPatternFreed = measure(() => {
  arrayElement = null;
});

let { objectProperty } = { objectProperty: new ArrayBuffer(SIZE) };
const objectPatternFreed = measure(() => {
  objectProperty = null;
});

let first = 1, middle = new ArrayBuffer(SIZE), last = 3;
const listFreed = measure(() => {
  middle = null;
});

let counter = 1;
counter++;
counter += 2;

const readEarly = () => early;
let early = new ArrayBuffer(SIZE);
const earlyClosureFreed = measure(() => {
  early = null;
});

const readEarlyPattern = () => earlyElement;
let [earlyElement] = [new ArrayBuffer(SIZE)];
const earlyPatternFreed = measure(() => {
  earlyElement = null;
});

const readCounter = () => counter;

describe("a reassigned top-level let binding", () => {
  test("releases a plain initializer", () => {
    expect(plainFreed).toBeGreaterThanOrEqual(SIZE);
    expect(plain).toBe(null);
  });

  test("releases an array pattern's initializer", () => {
    expect(arrayPatternFreed).toBeGreaterThanOrEqual(SIZE);
    expect(arrayElement).toBe(null);
  });

  test("releases an object pattern's initializer", () => {
    expect(objectPatternFreed).toBeGreaterThanOrEqual(SIZE);
    expect(objectProperty).toBe(null);
  });

  test("releases one initializer of a declaration list", () => {
    expect(listFreed).toBeGreaterThanOrEqual(SIZE);
    expect([first, middle, last]).toEqual([1, null, 3]);
  });

  test("releases the initializer a closure compiled before the declaration names", () => {
    expect(earlyClosureFreed).toBeGreaterThanOrEqual(SIZE);
    expect(readEarly()).toBe(null);
  });

  test("releases the initializer of a pattern a closure compiled before it names", () => {
    expect(earlyPatternFreed).toBeGreaterThanOrEqual(SIZE);
    expect(readEarlyPattern()).toBe(null);
  });
});

describe("a top-level let binding after its declaration", () => {
  test("is read and written through the global binding", () => {
    expect(counter).toBe(4);
    counter *= 2;
    expect(readCounter()).toBe(8);
  });
});
