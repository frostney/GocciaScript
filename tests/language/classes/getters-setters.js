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
