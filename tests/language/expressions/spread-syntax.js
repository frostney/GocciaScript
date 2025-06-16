/*---
description: Spread syntax edge cases, property ordering, and error conditions
features: [object-spread, array-spread, spread-overrides]
---*/

test("object spread with property ordering", () => {
  const obj1 = { a: 1, b: 2 };
  const obj2 = { c: 3, d: 4 };

  const combined = { ...obj1, ...obj2, b: 5, e: 6 };
  expect(combined).toEqual({ a: 1, b: 5, c: 3, d: 4, e: 6 });

  // Property order should be preserved
  const keys = Object.keys(combined);
  expect(keys).toEqual(["a", "b", "c", "d", "e"]);
});

test("object spread with property overrides", () => {
  const base = { name: "John", age: 30, active: true };

  // Later properties override earlier ones
  const updated = {
    ...base,
    age: 31,
    active: false,
    email: "john@example.com",
  };
  expect(updated).toEqual({
    name: "John",
    age: 31,
    active: false,
    email: "john@example.com",
  });

  // Multiple spreads with overrides
  const obj1 = { a: 1, b: 2 };
  const obj2 = { b: 3, c: 4 };
  const obj3 = { c: 5, d: 6 };

  const result = { ...obj1, ...obj2, ...obj3, a: 7 };
  expect(result).toEqual({ a: 7, b: 3, c: 5, d: 6 });
});

test("object spread with null and undefined", () => {
  // Spreading null and undefined should be ignored
  const result1 = { a: 1, ...null, b: 2 };
  expect(result1).toEqual({ a: 1, b: 2 });

  const result2 = { a: 1, ...undefined, b: 2 };
  expect(result2).toEqual({ a: 1, b: 2 });

  // Mixed with valid objects
  const result3 = { ...null, ...{ x: 1 }, ...undefined, y: 2 };
  expect(result3).toEqual({ x: 1, y: 2 });
});

test("object spread with non-enumerable properties", () => {
  const obj = {};
  Object.defineProperty(obj, "hidden", {
    value: "secret",
    enumerable: false,
    writable: true,
    configurable: true,
  });
  Object.defineProperty(obj, "visible", {
    value: "public",
    enumerable: true,
    writable: true,
    configurable: true,
  });

  const spread = { ...obj };
  expect(spread.visible).toBe("public");
  expect(spread.hidden).toBeUndefined();
  expect("hidden" in spread).toBe(false);
});

test("object spread with symbol properties", () => {
  const sym1 = Symbol("key1");
  const sym2 = Symbol("key2");

  const source = {
    regular: "value",
    [sym1]: "symbol1",
    [sym2]: "symbol2",
  };

  const spread = { ...source };
  expect(spread.regular).toBe("value");
  expect(spread[sym1]).toBe("symbol1");
  expect(spread[sym2]).toBe("symbol2");
});

test("object spread with getters and computed values", () => {
  let getterCallCount = 0;
  const source = {
    get dynamic() {
      getterCallCount++;
      return `computed-${getterCallCount}`;
    },
    static: "fixed",
  };

  // Getter should be called during spread
  const spread1 = { ...source };
  expect(spread1.dynamic).toBe("computed-1");
  expect(spread1.static).toBe("fixed");
  expect(getterCallCount).toBe(1);

  // Multiple spreads call getter multiple times
  const spread2 = { ...source, ...source };
  expect(spread2.dynamic).toBe("computed-3"); // Called twice more
  expect(getterCallCount).toBe(3);
});

test("array spread in different contexts", () => {
  const arr1 = [1, 2, 3];
  const arr2 = [4, 5];

  // Array literal context
  const combined = [...arr1, ...arr2, 6, 7];
  expect(combined).toEqual([1, 2, 3, 4, 5, 6, 7]);

  // Function call context
  const max = Math.max(...arr1, ...arr2);
  expect(max).toBe(5);

  // Mixed with individual elements
  const mixed = [0, ...arr1, 3.5, ...arr2, 8];
  expect(mixed).toEqual([0, 1, 2, 3, 3.5, 4, 5, 8]);
});

test("spread with iterable objects", () => {
  // String spreading
  const str = "hello";
  const spread = [...str];
  expect(spread).toEqual(["h", "e", "l", "l", "o"]);

  // Set spreading
  const set = new Set([1, 2, 3, 2, 1]);
  const fromSet = [...set];
  expect(fromSet).toEqual([1, 2, 3]);

  // Map keys/values spreading
  const map = new Map([
    ["a", 1],
    ["b", 2],
  ]);
  const entries = [...map];
  expect(entries).toEqual([
    ["a", 1],
    ["b", 2],
  ]);
});

test("nested spread operations", () => {
  const level1 = { a: 1, b: 2 };
  const level2 = { ...level1, c: 3 };
  const level3 = { ...level2, d: 4 };

  expect(level3).toEqual({ a: 1, b: 2, c: 3, d: 4 });

  // Array spreading with nested arrays
  const nested = [
    [1, 2],
    [3, 4],
  ];
  const flattened = [...nested[0], ...nested[1]];
  expect(flattened).toEqual([1, 2, 3, 4]);
});

test("spread with object method preservation", () => {
  const source = {
    name: "test",
    greet() {
      return `Hello, ${this.name}`;
    },
    arrow: () => "arrow function",
  };

  const spread = { ...source };
  expect(spread.name).toBe("test");
  expect(spread.greet()).toBe("Hello, test");
  expect(spread.arrow()).toBe("arrow function");
});

test("spread evaluation order with side effects", () => {
  let sideEffects = [];

  const obj1 = {
    get a() {
      sideEffects.push("get-a");
      return 1;
    },
  };

  const obj2 = {
    get b() {
      sideEffects.push("get-b");
      return 2;
    },
  };

  const result = {
    start: (() => {
      sideEffects.push("start");
      return 0;
    })(),
    ...obj1,
    middle: (() => {
      sideEffects.push("middle");
      return 1.5;
    })(),
    ...obj2,
    end: (() => {
      sideEffects.push("end");
      return 3;
    })(),
  };

  expect(result).toEqual({ start: 0, a: 1, middle: 1.5, b: 2, end: 3 });
  expect(sideEffects).toEqual(["start", "get-a", "middle", "get-b", "end"]);
});

test("spread with primitive type coercion", () => {
  // Number spreading (no enumerable properties)
  const num = { ...42 };
  expect(num).toEqual({});

  // Boolean spreading (no enumerable properties)
  const bool = { ...true };
  expect(bool).toEqual({});

  // String spreading (has indexed properties)
  const str = { ..."abc" };
  expect(str).toEqual({ 0: "a", 1: "b", 2: "c" });
});
