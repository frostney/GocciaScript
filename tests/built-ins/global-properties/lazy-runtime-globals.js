/*---
description: >
  Lazy runtime-extension globals (CSV/JSON5/JSONL/TOML/TSV/YAML), the lazy
  name-bound shims (Date and friends), and the lazy Goccia.semver namespace are
  observable as ordinary global properties and materialize correctly on first
  access, including a fresh named property read in bytecode mode.
features: [global-properties, globalThis]
---*/

const lazyDataFormats = ["CSV", "JSON5", "JSONL", "TOML", "TSV", "YAML"];
const lazyShims = [
  ["btoa", "function"],
  ["atob", "function"],
  ["parseInt", "function"],
  ["parseFloat", "function"],
  ["isNaN", "function"],
  ["isFinite", "function"],
  ["Date", "function"],
];

describe("lazy runtime globals: fresh named access", () => {
  test("a first named property read returns the backing object, not undefined", () => {
    // These named reads are the first touch of each global in this file. In
    // bytecode mode an unmaterialized lazy descriptor must materialize here
    // instead of reading through as undefined via the property inline cache.
    expect(typeof globalThis.CSV).toBe("object");
    expect(typeof globalThis.JSON5).toBe("object");
    expect(typeof globalThis.JSONL).toBe("object");
    expect(typeof globalThis.TOML).toBe("object");
    expect(typeof globalThis.TSV).toBe("object");
    expect(typeof globalThis.YAML).toBe("object");
    expect(typeof globalThis.Date).toBe("function");
    expect(typeof Goccia.semver).toBe("object");
  });
});

describe("lazy data-format globals", () => {
  test("each is a present, non-enumerable, writable, configurable own property", () => {
    for (const name of lazyDataFormats) {
      expect(name in globalThis).toBe(true);
      expect(Object.hasOwn(globalThis, name)).toBe(true);
      expect(Object.getOwnPropertyNames(globalThis).includes(name)).toBe(true);

      const desc = Object.getOwnPropertyDescriptor(globalThis, name);
      expect(desc.enumerable).toBe(false);
      expect(desc.writable).toBe(true);
      expect(desc.configurable).toBe(true);
    }
  });

  test("identifier and global-object bindings resolve to the same object", () => {
    expect(CSV === globalThis.CSV).toBe(true);
    expect(TOML === globalThis.TOML).toBe(true);
    expect(YAML === globalThis.YAML).toBe(true);
  });

  test("materialized namespaces are functional", () => {
    expect(JSON.stringify(CSV.parse("a,b\n1,2"))).toBe('[{"a":"1","b":"2"}]');
    expect(JSON.stringify(TSV.parse("a\tb\n1\t2"))).toBe('[{"a":"1","b":"2"}]');
    expect(JSON.stringify(TOML.parse("a = 1"))).toBe('{"a":1}');
    expect(JSON.stringify(JSON5.parse("{a:1,}"))).toBe('{"a":1}');
    expect(JSON.stringify(JSONL.parse('{"a":1}\n{"a":2}'))).toBe('[{"a":1},{"a":2}]');
    expect(JSON.stringify(YAML.parse("a: 1"))).toBe('{"a":1}');
    expect(Object.prototype.toString.call(CSV)).toBe("[object CSV]");
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
  test("a lazy global can be deleted from globalThis", () => {
    expect(delete globalThis.JSONL).toBe(true);
    expect("JSONL" in globalThis).toBe(false);
  });

  test("assigning a lazy global is not overwritten by later materialization", () => {
    globalThis.YAML = 123;
    expect(globalThis.YAML).toBe(123);
  });
});

describe("lazy Goccia.semver namespace", () => {
  test("is enumerable and present in Object.keys(Goccia)", () => {
    expect(Object.keys(Goccia).includes("semver")).toBe(true);
    expect(typeof Goccia.semver).toBe("object");
  });
});
