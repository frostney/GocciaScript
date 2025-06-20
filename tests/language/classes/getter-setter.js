/*---
description: Getter and setter
features: [getter-setter]
---*/

test("getter and setter", () => {
  class Calculator {
    constructor() {
      this._value = 0;
      this._operations = 0;
    }

    get value() {
      return this._value;
    }

    set value(newValue) {
      this._value = newValue;
      this._operations = this._operations + 1;
    }

    get doubled() {
      return this._value * 2;
    }

    get squared() {
      return this._value * this._value;
    }

    get operations() {
      return this._operations;
    }

    set reset(ignored) {
      this._value = 0;
      this._operations = 0;
    }

    add(n) {
      this._value = this._value + n;
      this._operations = this._operations + 1;
    }

    multiply(n) {
      this._value = this._value * n;
      this._operations = this._operations + 1;
    }
  }

  const calculator = new Calculator();
  expect(calculator.value).toBe(0);
  expect(calculator.operations).toBe(0);

  calculator.value = 5;
  expect(calculator.value).toBe(5);

  expect(calculator.doubled).toBe(10);
  expect(calculator.squared).toBe(25);

  calculator.add(3);
  expect(calculator.value).toBe(8);
  expect(calculator.operations).toBe(2);

  calculator.multiply(2);
  expect(calculator.value).toBe(16);
  expect(calculator.operations).toBe(3);

  calculator.reset = null;
  expect(calculator.value).toBe(0);
  expect(calculator.operations).toBe(4);
});
