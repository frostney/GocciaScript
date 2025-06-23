/*---
description: Object property access, method calls, and dynamic behavior work correctly
features: [Object, method-calls, property-access]
---*/

test("object method calls with parameters", () => {
  const obj = {
    method: (x) => {
      return "Method called with: " + x;
    },
    complexMethod: (a, b, c) => {
      return {
        params: [a, b, c],
        total: a + b + c,
        type: typeof a,
      };
    },
    chainableMethod: (value) => {
      this.lastValue = value;
      return this;
    },
    getValue: () => {
      return this.lastValue;
    },
  };

  // Test multiple calls with different parameters
  expect(obj.method("first")).toBe("Method called with: first");
  expect(obj.method("second")).toBe("Method called with: second");
  expect(obj.method(123)).toBe("Method called with: 123");

  // Test complex method calls
  const result = obj.complexMethod(10, 20, 30);
  expect(result.params).toEqual([10, 20, 30]);
  expect(result.total).toBe(60);
  expect(result.type).toBe("number");

  // Test method chaining
  obj.chainableMethod("test1").chainableMethod("test2");
  expect(obj.getValue()).toBe("test2");
});

test("object methods with context and binding", () => {
  const counter = {
    count: 0,
    increment: () => {
      this.count = this.count + 1;
      return this.count;
    },
    decrement: () => {
      this.count = this.count - 1;
      return this.count;
    },
    reset: () => {
      this.count = 0;
      return this;
    },
    getInfo: () => {
      return {
        current: this.count,
        isPositive: this.count > 0,
        isZero: this.count === 0,
      };
    },
  };

  expect(counter.increment()).toBe(1);
  expect(counter.increment()).toBe(2);
  expect(counter.increment()).toBe(3);

  expect(counter.getInfo()).toEqual({
    current: 3,
    isPositive: true,
    isZero: false,
  });

  expect(counter.decrement()).toBe(2);
  counter.reset();
  expect(counter.count).toBe(0);

  expect(counter.getInfo()).toEqual({
    current: 0,
    isPositive: false,
    isZero: true,
  });
});

test("object methods with chaining, nested calls and state", () => {
  const calculator = {
    result: 0,
    history: [],

    add: (value) => {
      this.result = this.result + value;
      this.history.push({ op: "add", value: value, result: this.result });
      return this;
    },

    multiply: (value) => {
      this.result = this.result * value;
      this.history.push({ op: "multiply", value: value, result: this.result });
      return this;
    },

    divide: (value) => {
      if (value !== 0) {
        this.result = this.result / value;
        this.history.push({ op: "divide", value: value, result: this.result });
      }
      return this;
    },

    getResult: () => {
      return this.result;
    },

    getHistory: () => {
      return this.history.slice(); // Return copy
    },

    clear: () => {
      this.result = 0;
      this.history = [];
      return this;
    },
  };

  // Test chained operations
  calculator.add(10).multiply(2).add(5).divide(5);
  expect(calculator.getResult()).toBe(5);

  const history = calculator.getHistory();
  expect(history).toHaveLength(4);
  expect(history[0]).toEqual({ op: "add", value: 10, result: 10 });
  expect(history[1]).toEqual({ op: "multiply", value: 2, result: 20 });
  expect(history[2]).toEqual({ op: "add", value: 5, result: 25 });
  expect(history[3]).toEqual({ op: "divide", value: 5, result: 5 });
});
