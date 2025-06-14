// Test static class properties
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
console.log("=== Static Property Tests ===");
console.log("MathUtils.PI:", MathUtils.PI);
console.log("MathUtils.E:", MathUtils.E);
console.log("MathUtils.version:", MathUtils.version);
console.log("MathUtils.config:", MathUtils.config);

// Test static property types
console.log("typeof MathUtils.PI:", typeof MathUtils.PI);
console.log("typeof MathUtils.version:", typeof MathUtils.version);
console.log("typeof MathUtils.config:", typeof MathUtils.config);

// Test static method still works
console.log("MathUtils.add(5, 3):", MathUtils.add(5, 3));

// Test instance creation and methods
const math = new MathUtils("MyMath");
console.log("math.getName():", math.getName());

// Test instance doesn't have static properties
console.log("math.PI:", math.PI); // Should be undefined
console.log("typeof math.PI:", typeof math.PI);

// Test with inheritance
class AdvancedMath extends MathUtils {
  static goldenRatio = 1.618;
  static author = "Advanced Team";

  static multiply(a, b) {
    return a * b;
  }
}

console.log("\n=== Inheritance Tests ===");
console.log("AdvancedMath.PI:", AdvancedMath.PI); // Inherited
console.log("AdvancedMath.goldenRatio:", AdvancedMath.goldenRatio); // Own
console.log("AdvancedMath.author:", AdvancedMath.author); // Own
console.log("AdvancedMath.multiply(4, 5):", AdvancedMath.multiply(4, 5));

// Test counter example
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

console.log("\n=== Counter Tests ===");
console.log("Initial Counter.totalInstances:", Counter.totalInstances);

const c1 = new Counter(10);
console.log("After c1, Counter.totalInstances:", Counter.totalInstances);

const c2 = new Counter();
console.log("After c2, Counter.totalInstances:", Counter.totalInstances);
console.log("c2.getValue():", c2.getValue());

const c3 = new Counter(42);
console.log("After c3, Counter.totalInstances:", Counter.totalInstances);
console.log("Counter.getTotal():", Counter.getTotal());

const ic = new InstanceCounter();
console.log("ic.getNumber():", ic.getNumber());
ic.increment();
console.log("ic.getNumber():", ic.getNumber());
