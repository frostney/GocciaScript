/*---
description: Object methods can return the receiver for chaining
features: [Object, method-calls, property-access]
---*/

test("object methods support chaining and accumulated state", () => {
  const calculator = {
    result: 0,
    history: [],

    add(value) {
      this.result = this.result + value;
      this.history.push({ op: "add", value, result: this.result });
      return this;
    },

    multiply(value) {
      this.result = this.result * value;
      this.history.push({ op: "multiply", value, result: this.result });
      return this;
    },

    divide(value) {
      if (value !== 0) {
        this.result = this.result / value;
        this.history.push({ op: "divide", value, result: this.result });
      }
      return this;
    },

    getResult() {
      return this.result;
    },

    getHistory() {
      return this.history.slice();
    },
  };

  calculator.add(10).multiply(2).add(5).divide(5);

  expect(calculator.getResult()).toBe(5);
  expect(calculator.getHistory()).toEqual([
    { op: "add", value: 10, result: 10 },
    { op: "multiply", value: 2, result: 20 },
    { op: "add", value: 5, result: 25 },
    { op: "divide", value: 5, result: 5 },
  ]);
});
