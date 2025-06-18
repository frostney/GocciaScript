/*---
description: Function parameters work correctly with various scenarios
features: [function-parameters, default-parameters]
---*/

test("function with no parameters", () => {
  const getConstant = () => {
    return 100;
  };
  expect(getConstant()).toBe(100);
});

test("function with single parameter", () => {
  const double = (x) => {
    return x * 2;
  };
  expect(double(7)).toBe(14);
});

test("function with multiple parameters", () => {
  const multiply = (a, b, c) => {
    return a * b * c;
  };
  expect(multiply(2, 3, 4)).toBe(24);
});

test("excess parameters are ignored", () => {
  const add = (a, b) => {
    return a + b;
  };
  expect(add(1, 2, 3, 4, 5)).toBe(3);
});

test("missing parameters are undefined", () => {
  const checkParams = (a, b, c) => {
    return [a, b, c];
  };
  const result = checkParams(1, 2);
  expect(result[0]).toBe(1);
  expect(result[1]).toBe(2);
  expect(result[2]).toBeUndefined();
});

test("default parameter single value", () => {
  const greet = (name = "World") => {
    return "Hello, " + name + "!";
  };
  expect(greet()).toBe("Hello, World!");
  expect(greet("Alice")).toBe("Hello, Alice!");
});

test("default parameter multiple values", () => {
  const greet = (name = "World", age = 20) => {
    return "Hello, " + name + "! You are " + age + " years old.";
  };
  expect(greet()).toBe("Hello, World! You are 20 years old.");
  expect(greet("Alice")).toBe("Hello, Alice! You are 20 years old.");
  expect(greet("Alice", 30)).toBe("Hello, Alice! You are 30 years old.");
});

test("default parameter with one required value", () => {
  const greet = (name, age = 20) => {
    return "Hello, " + name + "! You are " + age + " years old.";
  };
  expect(greet("Alice")).toBe("Hello, Alice! You are 20 years old.");
  expect(greet("Alice", 30)).toBe("Hello, Alice! You are 30 years old.");
});

test("default parameters with arrays", () => {
  const preFilledArray = ([x = 1, y = 2] = []) => {
    return x + y;
  };

  expect(preFilledArray()).toBe(3);
  expect(preFilledArray([])).toBe(3);
  expect(preFilledArray([2])).toBe(4);
  expect(preFilledArray([2, 3])).toBe(5);
});

test("default paramaters with object", () => {
  const greet = (name = "World", { age = 20, city = "New York" } = {}) => {
    return (
      "Hello, " +
      name +
      "! You are " +
      age +
      " years old and live in " +
      city +
      "."
    );
  };
  expect(greet()).toBe(
    "Hello, World! You are 20 years old and live in New York."
  );
  expect(greet("Alice")).toBe(
    "Hello, Alice! You are 20 years old and live in New York."
  );
  expect(greet("Alice", { age: 30, city: "Los Angeles" })).toBe(
    "Hello, Alice! You are 30 years old and live in Los Angeles."
  );
});

test("call time evaluation with arrays", () => {
  const append = (value, array = []) => {
    array.push(value);
    return array;
  };

  expect(append(1)).toEqual([1]);
  expect(append(2)).toEqual([2]);
});

test("call time evaluation with functions", () => {
  let numberOfTimesCalled = 0;
  const something = () => {
    numberOfTimesCalled += 1;
    return numberOfTimesCalled;
  };

  const callSomething = (thing = something()) => {
    return thing;
  };

  expect(callSomething()).toBe(1);
  expect(callSomething()).toBe(2);
});
