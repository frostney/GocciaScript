describe("Map subclassing", () => {
  test("new Map() creates empty map", () => {
    const m = new Map();
    expect(m.size).toBe(0);
  });

  test("new Map(iterable) populates from entries", () => {
    const m = new Map([["a", 1], ["b", 2]]);
    expect(m.size).toBe(2);
    expect(m.get("a")).toBe(1);
    expect(m.get("b")).toBe(2);
  });

  test("Map instanceof Map", () => {
    const m = new Map();
    expect(m instanceof Map).toBe(true);
  });

  test("class extends Map creates map instance", () => {
    class MyMap extends Map {}
    const m = new MyMap([["a", 1]]);
    expect(m.size).toBe(1);
    expect(m.get("a")).toBe(1);
  });

  test("subclass instanceof both Map and subclass", () => {
    class MyMap extends Map {}
    const m = new MyMap();
    expect(m instanceof Map).toBe(true);
    expect(m instanceof MyMap).toBe(true);
  });

  test("subclass has map methods", () => {
    class MyMap extends Map {}
    const m = new MyMap();
    m.set("x", 42);
    expect(m.has("x")).toBe(true);
    expect(m.get("x")).toBe(42);
  });

  test("subclass inherits Symbol.species from Map", () => {
    class MyMap extends Map {}
    expect(MyMap[Symbol.species]).toBe(MyMap);
  });
});

// ES2026 §24.1.1.1 Map step 2 creates the map from NewTarget before step 5
// reads its `set` adder, so a subclass's super(iterable) adds every entry
// through the subclass's own `set`. Expected values from Node.js v24.
describe("Map subclass super(iterable) adds through the subclass's set", () => {
  test("an overriding set runs once per entry", () => {
    const log = [];
    class LoggingMap extends Map {
      constructor() {
        super([[1, 2], [3, 4]]);
      }
      set(key, value) {
        log.push(`set:${key}`);
        return super.set(key, value);
      }
    }
    const m = new LoggingMap();
    expect(log).toEqual(["set:1", "set:3"]);
    expect(m.size).toBe(2);
    expect(m.get(1)).toBe(2);
  });

  test("a set patched onto the subclass prototype runs", () => {
    const log = [];
    class PatchedMap extends Map {
      constructor() {
        super([[1, 2]]);
      }
    }
    PatchedMap.prototype.set = ({
      set(key, value) {
        log.push(`patched:${key}`);
        return Map.prototype.set.call(this, key, value);
      },
    }).set;
    const m = new PatchedMap();
    expect(log).toEqual(["patched:1"]);
    expect(m.get(1)).toBe(2);
  });

  test("a set on new.target's prototype is the adder", () => {
    const log = [];
    class Sub extends Map {
      constructor(entries) {
        super(entries);
      }
    }
    class Other {}
    Other.prototype.set = ({
      set(key, value) {
        log.push(`other:${key}`);
        return Map.prototype.set.call(this, key, value);
      },
    }).set;
    const m = Reflect.construct(Sub, [[[5, 6]]], Other);
    expect(log).toEqual(["other:5"]);
    expect(Object.getPrototypeOf(m)).toBe(Other.prototype);
  });
});
