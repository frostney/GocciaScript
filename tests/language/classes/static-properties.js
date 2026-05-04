/*---
description: Static class properties and inheritance work correctly
features: [static-properties, static-methods]
---*/

test("static properties", () => {
  class MathUtils {
    static PI = 3.14159;
    static E = 2.71828;
    static version = "1.0.0";
    static config = { debug: true, maxValues: 100 };

    static add(a, b) {
      return a + b;
    }

    constructor(name) {
      this.name = name;
    }

    getName() {
      return this.name;
    }
  }

  // Test static property access
  expect(MathUtils.PI).toBe(3.14159);
  expect(MathUtils.E).toBe(2.71828);
  expect(MathUtils.version).toBe("1.0.0");
  expect(MathUtils.config).toEqual({ debug: true, maxValues: 100 });

  // Test static property types
  expect(typeof MathUtils.PI).toBe("number");
  expect(typeof MathUtils.version).toBe("string");
  expect(typeof MathUtils.config).toBe("object");

  // Test static method still works
  expect(MathUtils.add(5, 3)).toBe(8);

  // Test instance creation and methods
  const math = new MathUtils("MyMath");
  expect(math.getName()).toBe("MyMath");

  // Test instance doesn't have static properties
  expect(math.PI).toBeUndefined();
  expect(typeof math.PI).toBe("undefined");
});

test("static properties inheritance", () => {
  class MathUtils {
    static PI = 3.14159;
    static version = "1.0.0";

    static add(a, b) {
      return a + b;
    }
  }

  class AdvancedMath extends MathUtils {
    static goldenRatio = 1.618;
    static author = "Advanced Team";

    static multiply(a, b) {
      return a * b;
    }
  }

  // Test inherited static properties
  expect(AdvancedMath.PI).toBe(3.14159); // Inherited
  expect(AdvancedMath.goldenRatio).toBe(1.618); // Own
  expect(AdvancedMath.author).toBe("Advanced Team"); // Own
  expect(AdvancedMath.multiply(4, 5)).toBe(20);
});

test("static properties with instance counting", () => {
  class Counter {
    static totalInstances = 0;
    static defaultValue = 0;

    constructor(initialValue) {
      Counter.totalInstances = Counter.totalInstances + 1;
      this.value = initialValue || Counter.defaultValue;
    }

    getValue() {
      return this.value;
    }

    static getTotal() {
      return Counter.totalInstances;
    }
  }

  expect(Counter.totalInstances).toBe(0);

  const c1 = new Counter(10);
  expect(Counter.totalInstances).toBe(1);

  const c2 = new Counter();
  expect(Counter.totalInstances).toBe(2);
  expect(c2.getValue()).toBe(0);

  const c3 = new Counter(42);
  expect(Counter.totalInstances).toBe(3);
  expect(Counter.getTotal()).toBe(3);
});

test("instance properties vs static properties", () => {
  class InstanceCounter {
    number = 0;

    getNumber() {
      return this.number;
    }

    increment() {
      this.number = this.number + 1;
    }

    constructor() {
      this.number = 3;
    }
  }

  const ic = new InstanceCounter();
  expect(ic.getNumber()).toBe(3);
  ic.increment();
  expect(ic.getNumber()).toBe(4);
});
