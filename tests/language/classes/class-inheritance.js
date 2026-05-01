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

test("super() returns receiver unless superclass returns object", () => {
  class PrimitiveBase {
    constructor() {
      this.base = true;
      return 7;
    }
  }

  class PrimitiveDerived extends PrimitiveBase {
    constructor() {
      const value = super();
      this.superReturnedThis = value === this;
    }
  }

  class ObjectBase {
    constructor() {
      return { fromBase: true };
    }
  }

  class ObjectDerived extends ObjectBase {
    constructor() {
      const value = super();
      this.superReturnedObject = value.fromBase;
    }
  }

  const primitive = new PrimitiveDerived();
  expect(primitive.base).toBe(true);
  expect(primitive.superReturnedThis).toBe(true);

  const object = new ObjectDerived();
  expect(object.superReturnedObject).toBe(true);
});

test("extends rejects non-constructable callable superclass", () => {
  const NotConstructor = () => {};

  expect(() => {
    class Invalid extends NotConstructor {}
    return Invalid;
  }).toThrow(TypeError);
});

test("super.constructor reads the prototype constructor property", () => {
  class Base {}

  class Derived extends Base {
    getConstructor() {
      return super.constructor;
    }

    getComputedConstructor() {
      return super["constructor"];
    }
  }

  const derived = new Derived();
  expect(derived.getConstructor()).toBe(Base);
  expect(derived.getComputedConstructor()).toBe(Base);
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
