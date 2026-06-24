/*---
description: >
  Heavy built-in globals are exposed as ordinary global object properties even
  when their backing objects are materialized lazily.
features: [global-properties, globalThis]
---*/

const lazyGlobals = [
  ["Temporal", "object"],
  ["Intl", "object"],
  ["Atomics", "object"],
  ["Proxy", "function"],
  ["Reflect", "object"],
  ["DisposableStack", "function"],
  ["AsyncDisposableStack", "function"],
];

describe("lazy global built-ins", () => {
  test("lazy globals are own properties before direct access", () => {
    for (const [name] of lazyGlobals) {
      expect(name in globalThis).toBe(true);
      expect(Object.hasOwn(globalThis, name)).toBe(true);
    }
  });

  test("lazy globals materialize as ordinary writable configurable data properties", () => {
    for (const [name, expectedType] of lazyGlobals) {
      const desc = Object.getOwnPropertyDescriptor(globalThis, name);

      expect(desc !== undefined).toBe(true);
      expect(desc.enumerable).toBe(false);
      expect(desc.writable).toBe(true);
      expect(desc.configurable).toBe(true);
      expect(typeof desc.value).toBe(expectedType);
      expect(desc.value === globalThis[name]).toBe(true);
    }
  });

  test("lazy globals keep their declaration order on the global object", () => {
    const names = Object.getOwnPropertyNames(globalThis);
    let previousIndex = -1;

    for (const [name] of lazyGlobals) {
      const index = names.indexOf(name);

      expect(index > previousIndex).toBe(true);
      previousIndex = index;
    }
  });

  test("lazy global property values are identical to identifier bindings", () => {
    expect(globalThis.Temporal === Temporal).toBe(true);
    expect(globalThis.Intl === Intl).toBe(true);
    expect(globalThis.Atomics === Atomics).toBe(true);
    expect(globalThis.Proxy === Proxy).toBe(true);
    expect(globalThis.Reflect === Reflect).toBe(true);
    expect(globalThis.DisposableStack === DisposableStack).toBe(true);
    expect(globalThis.AsyncDisposableStack === AsyncDisposableStack).toBe(true);
  });

  test("configurable lazy globals can be deleted from globalThis", () => {
    expect(delete globalThis.Reflect).toBe(true);
    expect("Reflect" in globalThis).toBe(false);
  });
});
