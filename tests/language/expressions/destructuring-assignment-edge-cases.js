/*---
description: Destructuring assignment edge cases, error conditions, and complex patterns
features: [destructuring-assignment, default-parameters, rest-patterns]
---*/

test("destructuring assignment with null and undefined", () => {
  let x, y, z;

  // Null destructuring should throw TypeError
  expect(() => {
    ({ x } = null);
  }).toThrow(TypeError);

  expect(() => {
    ({ y } = undefined);
  }).toThrow(TypeError);

  // Array destructuring with null/undefined should throw
  expect(() => {
    [z] = null;
  }).toThrow(TypeError);

  expect(() => {
    [z] = undefined;
  }).toThrow(TypeError);
});

test("destructuring assignment with default values", () => {
  let a, b, c, d;

  // Object destructuring with defaults
  ({ a = 10, b = 20 } = { a: 5 });
  expect(a).toBe(5);
  expect(b).toBe(20);

  ({ c = "default", d = 42 } = { c: null, d: undefined });
  expect(c).toBe(null); // null doesn't trigger default
  expect(d).toBe(42); // undefined triggers default

  // Array destructuring with defaults
  [a, b = 100] = [1];
  expect(a).toBe(1);
  expect(b).toBe(100);

  [c = "fallback", d = "backup"] = [undefined, null];
  expect(c).toBe("fallback"); // undefined triggers default
  expect(d).toBe(null); // null doesn't trigger default
});

test("nested destructuring with complex patterns", () => {
  const data = {
    user: {
      name: "Alice",
      details: {
        age: 30,
        location: "NYC",
      },
    },
    scores: [95, 87, 92],
  };

  let name, age, location, firstScore, secondScore, remaining;

  // Nested object destructuring
  ({
    user: {
      name,
      details: { age, location },
    },
  } = data);
  expect(name).toBe("Alice");
  expect(age).toBe(30);
  expect(location).toBe("NYC");

  // Mixed object/array destructuring
  ({
    scores: [firstScore, secondScore, ...remaining],
  } = data);
  expect(firstScore).toBe(95);
  expect(secondScore).toBe(87);
  expect(remaining).toEqual([92]);

  // Error case: destructuring nested null
  expect(() => {
    ({
      user: {
        profile: { bio },
      },
    } = { user: { profile: null } });
  }).toThrow(TypeError);
});

test("destructuring assignment with rest patterns", () => {
  let first, second, rest, a, b, others;

  // Array rest patterns
  [first, second, ...rest] = [1, 2, 3, 4, 5];
  expect(first).toBe(1);
  expect(second).toBe(2);
  expect(rest).toEqual([3, 4, 5]);

  // Object rest patterns
  ({ a, b, ...others } = { a: 1, b: 2, c: 3, d: 4 });
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(others).toEqual({ c: 3, d: 4 });

  // Rest with empty remainder
  [first, ...rest] = [42];
  expect(first).toBe(42);
  expect(rest).toEqual([]);

  ({ a, ...others } = { a: 100 });
  expect(a).toBe(100);
  expect(others).toEqual({});
});

test("destructuring assignment with computed property names", () => {
  const key1 = "dynamicKey";
  const key2 = Symbol("test");
  let value1, value2, value3;

  const source = {
    [key1]: "dynamic value",
    [key2]: "symbol value",
    [1 + 1]: "computed value",
  };

  // Computed property destructuring
  ({ [key1]: value1, [key2]: value2, [2]: value3 } = source);
  expect(value1).toBe("dynamic value");
  expect(value2).toBe("symbol value");
  expect(value3).toBe("computed value");
});

test("destructuring assignment evaluation order", () => {
  let evalOrder = [];
  const tracker = {
    getProp: (name) => {
      evalOrder.push(`get-${name}`);
      return name;
    },
    setValue: (name, value) => {
      evalOrder.push(`set-${name}-${value}`);
      return value;
    },
  };

  let a, b;

  // Complex evaluation order test
  ({ [tracker.getProp("first")]: a, [tracker.getProp("second")]: b } = {
    first: tracker.setValue("a", 1),
    second: tracker.setValue("b", 2),
  });

  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(evalOrder).toEqual(["set-a-1", "set-b-2", "get-first", "get-second"]);
});

test("destructuring assignment with function parameters simulation", () => {
  // Simulating function parameter destructuring behavior
  const processUser = (userData) => {
    let name, age, email, preferences;
    ({ name, age, email = "not provided", ...preferences } = userData);

    return {
      name,
      age,
      email,
      preferences,
    };
  };

  const result1 = processUser({
    name: "John",
    age: 25,
    theme: "dark",
    notifications: true,
  });

  expect(result1).toEqual({
    name: "John",
    age: 25,
    email: "not provided",
    preferences: { theme: "dark", notifications: true },
  });

  const result2 = processUser({
    name: "Jane",
    age: 30,
    email: "jane@example.com",
    extra: "data",
  });

  expect(result2).toEqual({
    name: "Jane",
    age: 30,
    email: "jane@example.com",
    preferences: { extra: "data" },
  });
});

test("destructuring assignment with array-like objects", () => {
  const arrayLike = {
    0: "first",
    1: "second",
    2: "third",
    length: 3,
  };

  let a, b, rest;

  // Cannot destructure array-like as array directly - expect error
  expect(() => {
    [a, b, ...rest] = arrayLike;
  }).toThrow();

  // But can destructure as object with numeric keys
  ({ 0: a, 1: b, 2: rest } = arrayLike);
  expect(a).toBe("first");
  expect(b).toBe("second");
  expect(rest).toBe("third");
});
