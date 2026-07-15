/*---
description: >
  Global identifier reads observe live ordinary global object properties after
  the bytecode global-read inline cache has warmed.
features: [global-properties, globalThis]
---*/

const readCachedParseInt = () => parseInt;
const readCachedParseFloat = () => parseFloat;
const readCachedIsFinite = () => isFinite;
const readCachedReflect = () => Reflect;

describe("global identifier read cache", () => {
  test("re-reads the live value of a global object-backed binding", () => {
    const originalDescriptor = Object.getOwnPropertyDescriptor(
      globalThis,
      "parseInt",
    );
    const originalValue = readCachedParseInt();
    const replacement = () => 23;

    try {
      globalThis.parseInt = replacement;

      expect(readCachedParseInt()).toBe(replacement);
    } finally {
      Object.defineProperty(globalThis, "parseInt", originalDescriptor);
    }

    expect(readCachedParseInt()).toBe(originalValue);
  });

  test("falls back when a cached data property becomes an accessor", () => {
    const originalDescriptor = Object.getOwnPropertyDescriptor(
      globalThis,
      "parseInt",
    );
    const originalValue = readCachedParseInt();
    const replacement = () => 91;
    let getterCalls = 0;

    try {
      Object.defineProperty(globalThis, "parseInt", {
        configurable: true,
        get() {
          getterCalls++;
          return replacement;
        },
      });

      expect(readCachedParseInt()).toBe(replacement);
      expect(readCachedParseInt()).toBe(replacement);
      expect(getterCalls).toBe(2);
    } finally {
      Object.defineProperty(globalThis, "parseInt", originalDescriptor);
    }

    expect(readCachedParseInt()).toBe(originalValue);
  });

  test("invalidates the entry index after deletion and recreation", () => {
    const originalDescriptor = Object.getOwnPropertyDescriptor(
      globalThis,
      "parseFloat",
    );
    const originalValue = readCachedParseFloat();
    const replacement = () => 17;

    try {
      expect(delete globalThis.parseFloat).toBe(true);
      Object.defineProperty(globalThis, "parseFloat", {
        value: replacement,
        writable: true,
        configurable: true,
      });

      expect(readCachedParseFloat()).toBe(replacement);
    } finally {
      Object.defineProperty(globalThis, "parseFloat", originalDescriptor);
    }

    expect(readCachedParseFloat()).toBe(originalValue);
  });

  test("returns an accessor result before caching its replacement data", () => {
    const originalDescriptor = Object.getOwnPropertyDescriptor(
      globalThis,
      "isFinite",
    );
    const first = () => 31;
    const replacement = () => 47;
    let getterCalls = 0;

    try {
      Object.defineProperty(globalThis, "isFinite", {
        configurable: true,
        get() {
          getterCalls++;
          Object.defineProperty(globalThis, "isFinite", {
            value: replacement,
            writable: true,
            configurable: true,
          });
          return first;
        },
      });

      expect(readCachedIsFinite()).toBe(first);
      expect(readCachedIsFinite()).toBe(replacement);
      expect(getterCalls).toBe(1);
    } finally {
      Object.defineProperty(globalThis, "isFinite", originalDescriptor);
    }
  });

  test("caches a lazy built-in only after materialization", () => {
    const first = readCachedReflect();
    const second = readCachedReflect();

    expect(first === globalThis.Reflect).toBe(true);
    expect(second).toBe(first);
  });
});
