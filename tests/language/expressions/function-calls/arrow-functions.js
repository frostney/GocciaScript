/*---
description: Arrow function calls, context, and complex scenarios work correctly
features: [arrow-functions, function-calls, this-binding]
---*/

test("arrow function multiple calls with different parameter types", () => {
  const test = (x) => {
    return "Called with: " + x + " (type: " + typeof x + ")";
  };

  expect(test("first")).toBe("Called with: first (type: string)");
  expect(test("second")).toBe("Called with: second (type: string)");
  expect(test(123)).toBe("Called with: 123 (type: number)");
  expect(test(true)).toBe("Called with: true (type: boolean)");
  expect(test(null)).toBe("Called with: null (type: object)");
  expect(test(undefined)).toBe("Called with: undefined (type: undefined)");
});

test("arrow functions with multiple parameters and complex logic", () => {
  const calculator = (operation, a, b) => {
    switch (operation) {
      case "add":
        return a + b;
      case "subtract":
        return a - b;
      case "multiply":
        return a * b;
      case "divide":
        return b !== 0 ? a / b : "Division by zero";
      default:
        return "Unknown operation";
    }
  };

  expect(calculator("add", 5, 3)).toBe(8);
  expect(calculator("subtract", 10, 4)).toBe(6);
  expect(calculator("multiply", 3, 7)).toBe(21);
  expect(calculator("divide", 15, 3)).toBe(5);
  expect(calculator("divide", 10, 0)).toBe("Division by zero");
  expect(calculator("modulo", 10, 3)).toBe("Unknown operation");
});

test("object method calls with arrow functions and context", () => {
  const obj = {
    name: "TestObject",
    value: 42,

    // Arrow function - doesn't bind 'this'
    arrowMethod: (x) => {
      return "Arrow method called with: " + x;
    },

    // Regular function - binds 'this'
    regularMethod: function (x) {
      return "Regular method called with: " + x + " on " + this.name;
    },

    // Method that calls other methods
    combinedMethod: function (x) {
      const arrowResult = this.arrowMethod(x);
      const regularResult = this.regularMethod(x);
      return {
        arrow: arrowResult,
        regular: regularResult,
        value: this.value,
      };
    },
  };

  expect(obj.arrowMethod("test1")).toBe("Arrow method called with: test1");
  expect(obj.regularMethod("test2")).toBe(
    "Regular method called with: test2 on TestObject"
  );

  const combined = obj.combinedMethod("test3");
  expect(combined.arrow).toBe("Arrow method called with: test3");
  expect(combined.regular).toBe(
    "Regular method called with: test3 on TestObject"
  );
  expect(combined.value).toBe(42);
});

test("arrow function closures and variable capture", () => {
  const createCounter = (initialValue = 0) => {
    let count = initialValue;

    return {
      increment: () => ++count,
      decrement: () => --count,
      get: () => count,
      reset: () => {
        count = initialValue;
        return count;
      },
      add: (value) => {
        count += value;
        return count;
      },
    };
  };

  const counter1 = createCounter(5);
  const counter2 = createCounter(10);

  expect(counter1.get()).toBe(5);
  expect(counter2.get()).toBe(10);

  expect(counter1.increment()).toBe(6);
  expect(counter1.increment()).toBe(7);
  expect(counter2.decrement()).toBe(9);

  expect(counter1.add(3)).toBe(10);
  expect(counter2.add(-5)).toBe(4);

  expect(counter1.reset()).toBe(5);
  expect(counter2.reset()).toBe(10);
});

test("arrow functions as callbacks and higher-order functions", () => {
  const numbers = [1, 2, 3, 4, 5];

  // Test arrow functions with array methods
  const doubled = numbers.map((x) => x * 2);
  expect(doubled).toEqual([2, 4, 6, 8, 10]);

  const filtered = numbers.filter((x) => x > 3);
  expect(filtered).toEqual([4, 5]);

  const sum = numbers.reduce((acc, x) => acc + x, 0);
  expect(sum).toBe(15);

  // Test custom higher-order function
  const processArray = (arr, processor) => {
    return arr.map(processor);
  };

  const squared = processArray(numbers, (x) => x * x);
  expect(squared).toEqual([1, 4, 9, 16, 25]);

  const stringified = processArray(numbers, (x) => `Number: ${x}`);
  expect(stringified).toEqual([
    "Number: 1",
    "Number: 2",
    "Number: 3",
    "Number: 4",
    "Number: 5",
  ]);
});

test("nested arrow functions and complex scenarios", () => {
  const createMultiplier = (factor) => {
    return (base) => {
      return (times) => {
        return base * factor * times;
      };
    };
  };

  const doubleMultiplier = createMultiplier(2);
  const tripleMultiplier = createMultiplier(3);

  expect(doubleMultiplier(5)(3)).toBe(30); // 5 * 2 * 3
  expect(tripleMultiplier(4)(2)).toBe(24); // 4 * 3 * 2

  // Test function composition
  const compose = (f, g) => (x) => f(g(x));
  const addOne = (x) => x + 1;
  const double = (x) => x * 2;

  const addThenDouble = compose(double, addOne);
  const doubleThenAdd = compose(addOne, double);

  expect(addThenDouble(5)).toBe(12); // (5 + 1) * 2
  expect(doubleThenAdd(5)).toBe(11); // (5 * 2) + 1
});
