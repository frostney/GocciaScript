/*---
description: Getter and setter
features: [getter-setter]
---*/

test("getter and setter", () => {
  const calculator = {
    _value: 0,
    _operations: 0,

    // Basic getter and setter
    get value() {
      return this._value;
    },

    set value(newValue) {
      this._value = newValue;
      this._operations = this._operations + 1;
    },

    // Computed properties
    get doubled() {
      return this._value * 2;
    },

    get squared() {
      return this._value * this._value;
    },

    get operations() {
      return this._operations;
    },

    // Property with side effects
    set reset(ignored) {
      this._operations = this._operations + 1;
      this._value = 0;
    },

    // Regular methods that use getters/setters
    add(n) {
      this.value = this.value + n;
      return this.value;
    },

    multiply(n) {
      this.value = this.value * n;
      return this.value;
    },
  };

  expect(calculator.value).toBe(0);
  expect(calculator.operations).toBe(0);

  calculator.value = 5;
  expect(calculator.value).toBe(5);
  expect(calculator.operations).toBe(1);

  expect(calculator.doubled).toBe(10);
  expect(calculator.squared).toBe(25);

  calculator.add(3);
  expect(calculator.value).toBe(8);
  expect(calculator.operations).toBe(2);

  calculator.multiply(2);
  expect(calculator.value).toBe(16);
  expect(calculator.operations).toBe(3);

  calculator.reset = null; // Value doesn't matter, setter does the work
  expect(calculator.value).toBe(0);
  expect(calculator.operations).toBe(4);
});
