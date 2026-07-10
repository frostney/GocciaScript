/*---
description: >
  Runtime-extension APIs that moved to goccia:* modules are absent from the
  ambient global surface, while lazy name-bound shims (Date and friends)
  remain observable as ordinary global properties and materialize correctly on
  first access, including a fresh named property read in bytecode mode.
features: [global-properties, globalThis]
---*/

const runtimeModuleGlobalNames = [
  "CSV",
  "JSON5",
  "JSONL",
  "TOML",
  "TSV",
  "YAML",
];
const lazyShims = [
  ["btoa", "function"],
  ["atob", "function"],
  ["parseInt", "function"],
  ["parseFloat", "function"],
  ["isNaN", "function"],
  ["isFinite", "function"],
  ["Date", "function"],
];

describe("runtime module APIs are not ambient globals", () => {
  test("data-format global properties are absent", () => {
    for (const name of runtimeModuleGlobalNames) {
      expect(name in globalThis).toBe(false);
      expect(Object.hasOwn(globalThis, name)).toBe(false);
      expect(Object.getOwnPropertyDescriptor(globalThis, name)).toBe(undefined);
      expect(Object.getOwnPropertyNames(globalThis).includes(name)).toBe(false);
    }
  });

  test("data-format identifiers are absent", () => {
    expect(typeof CSV).toBe("undefined");
    expect(typeof JSON5).toBe("undefined");
    expect(typeof JSONL).toBe("undefined");
    expect(typeof TOML).toBe("undefined");
    expect(typeof TSV).toBe("undefined");
    expect(typeof YAML).toBe("undefined");
  });

  test("Goccia.semver is absent", () => {
    expect("semver" in Goccia).toBe(false);
    expect(Object.hasOwn(Goccia, "semver")).toBe(false);
    expect(Object.keys(Goccia).includes("semver")).toBe(false);
    expect(Goccia.semver).toBe(undefined);
  });
});

describe("lazy name-bound shims: fresh named access", () => {
  test("a first named property read returns the backing object, not undefined", () => {
    // These named reads are the first touch of each shim in this file. In
    // bytecode mode an unmaterialized lazy descriptor must materialize here
    // instead of reading through as undefined via the property inline cache.
    expect(typeof globalThis.Date).toBe("function");
  });
});

describe("lazy name-bound shims", () => {
  test("each is a present, non-enumerable, writable, configurable own property", () => {
    for (const [name, expectedType] of lazyShims) {
      expect(name in globalThis).toBe(true);
      expect(Object.hasOwn(globalThis, name)).toBe(true);
      expect(Object.getOwnPropertyNames(globalThis).includes(name)).toBe(true);
      expect(typeof globalThis[name]).toBe(expectedType);

      const desc = Object.getOwnPropertyDescriptor(globalThis, name);
      expect(desc.enumerable).toBe(false);
      expect(desc.writable).toBe(true);
      expect(desc.configurable).toBe(true);
    }
  });

  test("shim behavior is preserved after lazy materialization", () => {
    expect(parseInt("42px")).toBe(42);
    expect(parseFloat("3.14x")).toBe(3.14);
    expect(isNaN("nope")).toBe(true);
    expect(isFinite(5)).toBe(true);
    expect(atob(btoa("hello"))).toBe("hello");
    expect(new Date(0).toISOString()).toBe("1970-01-01T00:00:00.000Z");
    expect(globalThis.Date === Date).toBe(true);
  });

  test("eager Object.prototype shims still install at boot", () => {
    expect(({ a: 1 }).hasOwnProperty("a")).toBe(true);
    expect(({}).__proto__ === Object.prototype).toBe(true);
  });
});

describe("lazy globals stay configurable and writable", () => {
  test("a lazy shim can be deleted from globalThis", () => {
    expect(delete globalThis.Date).toBe(true);
    expect("Date" in globalThis).toBe(false);
  });
});
