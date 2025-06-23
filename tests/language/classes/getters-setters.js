/*---
description: Class getters and setters work correctly
features: [class-getters, class-setters]
---*/

test("basic getters and setters", () => {
  class Rectangle {
    constructor(width, height) {
      this._width = width;
      this._height = height;
    }

    get width() {
      return this._width;
    }

    set width(value) {
      if (value > 0) {
        this._width = value;
      }
    }

    get height() {
      return this._height;
    }

    set height(value) {
      if (value > 0) {
        this._height = value;
      }
    }

    get area() {
      return this._width * this._height;
    }
  }

  const rect = new Rectangle(5, 10);
  expect(rect.width).toBe(5);
  expect(rect.height).toBe(10);
  expect(rect.area).toBe(50);

  rect.width = 8;
  expect(rect.width).toBe(8);
  expect(rect.area).toBe(80);

  // Invalid values are ignored
  rect.width = -5;
  expect(rect.width).toBe(8); // Unchanged
});

test("computed property getters", () => {
  class Circle {
    constructor(radius) {
      this.radius = radius;
    }

    get diameter() {
      return this.radius * 2;
    }

    get circumference() {
      return 2 * Math.PI * this.radius;
    }

    get area() {
      return Math.PI * this.radius * this.radius;
    }
  }

  const circle = new Circle(3);
  expect(circle.diameter).toBe(6);
  expect(circle.circumference).toBeCloseTo(18.85, 2);
  expect(circle.area).toBeCloseTo(28.27, 2);
});

test("complex example", () => {
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
      this._operations = this._operations + 1;
      this._value = 0;
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
