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

test("default parameters with function definition", () => {
  const greet = (nameFn = () => "World") => {
    return "Hello, " + nameFn() + "!";
  };
  expect(greet()).toBe("Hello, World!");
  expect(greet(() => "Alice")).toBe("Hello, Alice!");
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

test("every argument count reaches the parameters and the rest parameter", () => {
  const collect = (a, b, c, ...rest) => [a, b, c, rest.length, rest.join("")].join("|");
  const values = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"];

  expect(collect()).toBe("|||0|");
  expect(collect("a")).toBe("a|||0|");
  expect(collect("a", "b", "c")).toBe("a|b|c|0|");
  expect(collect("a", "b", "c", "d")).toBe("a|b|c|1|d");
  expect(collect("a", "b", "c", "d", "e", "f", "g", "h")).toBe("a|b|c|5|defgh");
  expect(collect("a", "b", "c", "d", "e", "f", "g", "h", "i")).toBe("a|b|c|6|defghi");
  expect(collect(...values)).toBe("a|b|c|9|defghijkl");
});

test("arguments beyond the parameters do not leak into the callee's locals", () => {
  const none = () => {
    let first;
    let second;
    return [first, second];
  };
  const one = (a) => {
    let first;
    let second;
    let third;
    return [a, first, second, third];
  };

  expect(none(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).toEqual([undefined, undefined]);
  expect(one(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).toEqual([1, undefined, undefined, undefined]);
});

test("a method keeps its receiver and arguments apart for any argument count", () => {
  const holder = {
    tag: "T",
    join(...parts) {
      return this.tag + parts.join("");
    },
    pair(a, b) {
      return [this.tag, a, b];
    },
  };

  expect(holder.join()).toBe("T");
  expect(holder.join(1)).toBe("T1");
  expect(holder.join(1, 2, 3, 4)).toBe("T1234");
  expect(holder.join(1, 2, 3, 4, 5, 6, 7, 8, 9)).toBe("T123456789");
  expect(holder.pair()).toEqual(["T", undefined, undefined]);
  expect(holder.pair(1, 2, 3, 4, 5, 6)).toEqual(["T", 1, 2]);
});

test("arguments captured by a closure survive the call", () => {
  const capture = (a, b, c, d, e) => [() => a, () => e, () => (c = c + 1)];
  const [first, last, bump] = capture(1, 2, 3, 4, 5, 6, 7);
  const other = capture(10, 20, 30, 40, 50);

  expect(first()).toBe(1);
  expect(last()).toBe(5);
  expect(bump()).toBe(4);
  expect(bump()).toBe(5);
  expect(other[0]()).toBe(10);
  expect(other[2]()).toBe(31);
  expect(first()).toBe(1);
});
