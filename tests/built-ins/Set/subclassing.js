describe("Set subclassing", () => {
  test("new Set() creates empty set", () => {
    const s = new Set();
    expect(s.size).toBe(0);
  });

  test("new Set(iterable) populates from values", () => {
    const s = new Set([1, 2, 3]);
    expect(s.size).toBe(3);
    expect(s.has(1)).toBe(true);
    expect(s.has(2)).toBe(true);
    expect(s.has(3)).toBe(true);
  });

  test("Set instanceof Set", () => {
    const s = new Set();
    expect(s instanceof Set).toBe(true);
  });

  test("class extends Set creates set instance", () => {
    class MySet extends Set {}
    const s = new MySet([1, 2, 3]);
    expect(s.size).toBe(3);
    expect(s.has(1)).toBe(true);
  });

  test("subclass instanceof both Set and subclass", () => {
    class MySet extends Set {}
    const s = new MySet();
    expect(s instanceof Set).toBe(true);
    expect(s instanceof MySet).toBe(true);
  });

  test("subclass has set methods", () => {
    class MySet extends Set {}
    const s = new MySet();
    s.add(42);
    expect(s.has(42)).toBe(true);
    expect(s.size).toBe(1);
  });

  test("subclass inherits Symbol.species from Set", () => {
    class MySet extends Set {}
    expect(MySet[Symbol.species]).toBe(MySet);
  });
});

// ES2026 §24.2.2.1 Set step 2 creates the set from NewTarget before step 5
// reads its `add` adder, so a subclass's super(iterable) adds every value
// through the subclass's own `add`. Expected values from Node.js v24.
describe("Set subclass super(iterable) adds through the subclass's add", () => {
  test("an overriding add runs once per value", () => {
    const log = [];
    class LoggingSet extends Set {
      constructor() {
        super([1, 2]);
      }
      add(value) {
        log.push(`add:${value}`);
        return super.add(value);
      }
    }
    const s = new LoggingSet();
    expect(log).toEqual(["add:1", "add:2"]);
    expect(s.size).toBe(2);
  });

  test("an add reached through a Proxy new.target runs", () => {
    const log = [];
    class LoggingSet extends Set {
      constructor(values) {
        super(values);
      }
      add(value) {
        log.push(`add:${value}`);
        return super.add(value);
      }
    }
    const s = Reflect.construct(LoggingSet, [[7]], new Proxy(LoggingSet, {}));
    expect(log).toEqual(["add:7"]);
    expect(Object.getPrototypeOf(s)).toBe(LoggingSet.prototype);
  });
});
