/*---
description: Class inheritance with extends keyword works correctly
features: [class-inheritance, extends]
---*/

test("basic inheritance with extends", () => {
  class Animal {
    constructor(name) {
      this.name = name;
    }

    speak() {
      return `${this.name} makes a sound`;
    }
  }

  class Dog extends Animal {
    speak() {
      return `${this.name} barks`;
    }
  }

  const dog = new Dog("Rex");
  expect(dog.name).toBe("Rex");
  expect(dog.speak()).toBe("Rex barks");
  expect(dog instanceof Dog).toBeTruthy();
  expect(dog instanceof Animal).toBeTruthy();
});

test("super() in constructor", () => {
  class Vehicle {
    constructor(brand, year) {
      this.brand = brand;
      this.year = year;
    }
  }

  class Car extends Vehicle {
    constructor(brand, year, doors) {
      super(brand, year);
      this.doors = doors;
    }

    getInfo() {
      return `${this.year} ${this.brand} with ${this.doors} doors`;
    }
  }

  const car = new Car("Toyota", 2020, 4);
  expect(car.brand).toBe("Toyota");
  expect(car.year).toBe(2020);
  expect(car.doors).toBe(4);
  expect(car.getInfo()).toBe("2020 Toyota with 4 doors");
});

test("super method calls", () => {
  class Shape {
    constructor(name) {
      this.name = name;
    }

    describe() {
      return `This is a ${this.name}`;
    }
  }

  class Circle extends Shape {
    constructor(radius) {
      super("circle");
      this.radius = radius;
    }

    describe() {
      return super.describe() + ` with radius ${this.radius}`;
    }

    area() {
      return Math.PI * this.radius * this.radius;
    }
  }

  const circle = new Circle(5);
  expect(circle.describe()).toBe("This is a circle with radius 5");
  expect(circle.area()).toBeCloseTo(78.54, 2);
});
