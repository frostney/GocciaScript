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
    field = "field";

    constructor() {
      const value = super();
      this.superReturnedThis = value === this;
      this.superReturnedObject = value.fromBase;
      return this;
    }
  }

  const primitive = new PrimitiveDerived();
  expect(primitive.base).toBe(true);
  expect(primitive.superReturnedThis).toBe(true);

  const object = new ObjectDerived();
  expect(object.fromBase).toBe(true);
  expect(object.field).toBe("field");
  expect(object.superReturnedThis).toBe(true);
  expect(object.superReturnedObject).toBe(true);
});

test("own constructor object return does not replay fields", () => {
  class BaseReturn {
    field = "base-field";

    constructor() {
      return {};
    }
  }

  class Parent {}
  class DerivedReturn extends Parent {
    field = "derived-field";

    constructor() {
      super();
      return {};
    }
  }

  expect(new BaseReturn().field).toBeUndefined();
  expect(new DerivedReturn().field).toBeUndefined();
});

test("super replacement returned as this receives fields", () => {
  class Base {
    constructor() {
      return { fromBase: true };
    }
  }

  class Derived extends Base {
    field = "field";

    constructor() {
      super();
      return this;
    }
  }

  const derived = new Derived();
  expect(derived.fromBase).toBe(true);
  expect(derived.field).toBe("field");
});

test("derived constructor returning primitive throws TypeError", () => {
  class Base {
    constructor() {
      this.ready = true;
      return 7;
    }
  }

  const base = new Base();
  expect(base.ready).toBe(true);

  class Derived extends Base {
    constructor() {
      super();
      return 7;
    }
  }

  expect(() => new Derived()).toThrow(TypeError);
});

test("super() throws when a derived superclass constructor returns primitive", () => {
  class Base {}

  class Middle extends Base {
    constructor() {
      super();
      return 7;
    }
  }

  class Leaf extends Middle {
    constructor() {
      super();
    }
  }

  expect(() => new Leaf()).toThrow(TypeError);
});

test("default constructor throws when a derived superclass constructor returns primitive", () => {
  class Base {}

  class Middle extends Base {
    constructor() {
      super();
      return 7;
    }
  }

  class Leaf extends Middle {}

  expect(() => new Leaf()).toThrow(TypeError);
});

test("super() replacement receives superclass initializers", () => {
  let leafPrototype;

  class Base {
    baseField = "base";

    constructor() {
      return Object.create(leafPrototype);
    }
  }

  class Middle extends Base {
    constructor() {
      return super();
    }
  }

  class Leaf extends Middle {
    readBase() {
      return this.baseField;
    }
  }

  leafPrototype = Leaf.prototype;

  const leaf = new Leaf();
  expect(leaf.readBase()).toBe("base");
});

test("implicit super preserves derived constructor receiver replacement", () => {
  let leafPrototype;

  class Base {
    constructor() {
      const replacement = Object.create(leafPrototype);
      replacement.marker = "replacement";
      return replacement;
    }
  }

  class Middle extends Base {
    constructor() {
      super();
    }
  }

  class Leaf extends Middle {
    readMarker() {
      return this.marker;
    }
  }

  leafPrototype = Leaf.prototype;

  const leaf = new Leaf();
  expect(leaf.readMarker()).toBe("replacement");
});

test("super() returning this does not replay superclass initializers", () => {
  let initializersRun = 0;

  class Base {
    value = ++initializersRun;

    constructor() {
      return this;
    }
  }

  class Derived extends Base {
    constructor() {
      super();
    }
  }

  const instance = new Derived();
  expect(instance.value).toBe(1);
  expect(initializersRun).toBe(1);
});

test("implicit super returning this does not replay superclass initializers", () => {
  let initializersRun = 0;

  class Base {
    value = ++initializersRun;

    constructor() {
      return this;
    }
  }

  class Derived extends Base {}

  const instance = new Derived();
  expect(instance.value).toBe(1);
  expect(initializersRun).toBe(1);
});

test("bound class super runs constructor and field initializers", () => {
  class Base {
    field = "field";

    constructor(value) {
      this.value = value;
    }
  }

  const BoundBase = Base.bind(null, 41);
  BoundBase.prototype = Base.prototype;

  class Derived extends BoundBase {}

  const instance = new Derived();
  expect(instance.field).toBe("field");
  expect(instance.value).toBe(41);
});

test("bound class super without constructor runs field initializers", () => {
  class Base {
    field = "field";
  }

  const BoundBase = Base.bind(null);
  BoundBase.prototype = Base.prototype;

  class Derived extends BoundBase {}

  const instance = new Derived();
  expect(instance.field).toBe("field");
});

test("bound class super replacement does not replay superclass initializers", () => {
  let derivedPrototype;
  let initializersRun = 0;

  class Base {
    field = ++initializersRun;

    constructor() {
      return Object.create(derivedPrototype);
    }
  }

  const BoundBase = Base.bind(null);
  BoundBase.prototype = Base.prototype;

  class Derived extends BoundBase {
    readField() {
      return this.field;
    }
  }

  derivedPrototype = Derived.prototype;

  const instance = new Derived();
  expect(instance.readField()).toBeUndefined();
  expect(initializersRun).toBe(1);
});

test("closure captured this before super observes initialized receiver", () => {
  class Base {}

  class Derived extends Base {
    constructor() {
      const readThis = () => this;
      super();
      this.marker = "initialized";
      expect(readThis()).toBe(this);
    }
  }

  const instance = new Derived();
  expect(instance.marker).toBe("initialized");
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

test("super.constructor member calls are ordinary class calls", () => {
  class Base {
    constructor() {
      this.value = 1;
    }
  }

  class DotDerived extends Base {
    constructor() {
      super.constructor();
    }
  }

  class ComputedDerived extends Base {
    constructor() {
      super["constructor"]();
    }
  }

  expect(() => new DotDerived()).toThrow(TypeError);
  expect(() => new ComputedDerived()).toThrow(TypeError);
});

test("static super observes ordinary own properties on intermediate constructors", () => {
  class Base {
    static shadowed() {
      return "base";
    }
  }

  class Middle extends Base {}
  Middle.shadowed = "middle";

  class Derived extends Middle {
    static readShadowed() {
      return super.shadowed;
    }
  }

  expect(Derived.readShadowed()).toBe("middle");
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
