/*---
description: Computed destructuring
features: [destructuring]
---*/

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
      name: name,
      age: age,
      email: email,
      preferences: preferences,
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
