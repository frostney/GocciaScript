/*---
description: Private fields edge cases, property enumeration, and access patterns
features: [private-fields, private-methods, class-static-fields-private]
---*/

test("private fields are not enumerable properties", () => {
  class TestClass {
    #privateField = "private";
    publicField = "public";

    getPrivate() {
      return this.#privateField;
    }
  }

  const instance = new TestClass();

  // Private fields should not appear in property enumerations
  expect(Object.hasOwn(TestClass.prototype, "#privateField")).toBe(false);
  expect(Object.hasOwn(TestClass, "#privateField")).toBe(false);
  expect(Object.hasOwn(instance, "#privateField")).toBe(false);

  // Public fields should be enumerable
  expect(Object.hasOwn(instance, "publicField")).toBe(true);

  // Private field should be accessible through methods
  expect(instance.getPrivate()).toBe("private");

  // Private fields should not show up in Object.keys, Object.values, etc.
  const keys = Object.keys(instance);
  expect(keys).toContain("publicField");
  expect(keys).not.toContain("#privateField");
  expect(keys).not.toContain("privateField");
});

test("private fields with inheritance and shadowing", () => {
  class Base {
    #basePrivate = "base";

    getBasePrivate() {
      return this.#basePrivate;
    }

    setBasePrivate(value) {
      this.#basePrivate = value;
    }
  }

  class Derived extends Base {
    #derivedPrivate = "derived";
    #basePrivate = "shadowed"; // This should be a different field

    getDerivedPrivate() {
      return this.#derivedPrivate;
    }

    getShadowedPrivate() {
      return this.#basePrivate;
    }
  }

  const instance = new Derived();

  // Base private field should still be accessible through base methods
  expect(instance.getBasePrivate()).toBe("base");

  // Derived private field should be accessible
  expect(instance.getDerivedPrivate()).toBe("derived");

  // Shadowed private field should be separate
  expect(instance.getShadowedPrivate()).toBe("shadowed");

  // Modifying base private through base method
  instance.setBasePrivate("modified");
  expect(instance.getBasePrivate()).toBe("modified");
  expect(instance.getShadowedPrivate()).toBe("shadowed"); // Should not change
});

test("static private fields", () => {
  class TestClass {
    static #staticPrivate = "static value";
    #instancePrivate = "instance value";

    static getStaticPrivate() {
      return TestClass.#staticPrivate;
    }

    static setStaticPrivate(value) {
      TestClass.#staticPrivate = value;
    }

    getInstancePrivate() {
      return this.#instancePrivate;
    }

    // Static method cannot access instance private fields
    static tryAccessInstance() {
      // This would throw an error if uncommented:
      // return this.#instancePrivate;
      return "cannot access";
    }
  }

  // Static private field access
  expect(TestClass.getStaticPrivate()).toBe("static value");

  TestClass.setStaticPrivate("modified static");
  expect(TestClass.getStaticPrivate()).toBe("modified static");

  // Instance method can access instance private
  const instance = new TestClass();
  expect(instance.getInstancePrivate()).toBe("instance value");

  // Static private fields are not enumerable on class
  expect(Object.hasOwn(TestClass, "#staticPrivate")).toBe(false);
  expect(TestClass.tryAccessInstance()).toBe("cannot access");
});

test("private fields with complex initialization", () => {
  let initOrder = [];

  class TestClass {
    #first = (() => {
      initOrder.push("first");
      return 1;
    })();
    #second = (() => {
      initOrder.push("second");
      return 2;
    })();
    #computed = this.#first + this.#second;

    constructor(extra) {
      initOrder.push("constructor");
      this.#extra = extra;
    }

    #extra;

    getValues() {
      return {
        first: this.#first,
        second: this.#second,
        computed: this.#computed,
        extra: this.#extra,
      };
    }
  }

  const instance = new TestClass(10);
  const values = instance.getValues();

  expect(values).toEqual({
    first: 1,
    second: 2,
    computed: 3,
    extra: 10,
  });

  expect(initOrder).toEqual(["first", "second", "constructor"]);
});

test("private methods and getters/setters", () => {
  class TestClass {
    #value = 0;

    #validate(val) {
      return typeof val === "number" && val >= 0;
    }

    #transform(val) {
      return val * 2;
    }

    get #computed() {
      return this.#value * 10;
    }

    set #computed(val) {
      if (this.#validate(val)) {
        this.#value = val / 10;
      }
    }

    setValue(val) {
      if (this.#validate(val)) {
        this.#value = this.#transform(val);
      }
    }

    getValue() {
      return this.#value;
    }

    getComputed() {
      return this.#computed;
    }

    setComputed(val) {
      this.#computed = val;
    }
  }

  const instance = new TestClass();

  // Test private method usage
  instance.setValue(5);
  expect(instance.getValue()).toBe(10); // 5 * 2

  // Test private getter
  expect(instance.getComputed()).toBe(100); // 10 * 10

  // Test private setter
  instance.setComputed(50);
  expect(instance.getValue()).toBe(5); // 50 / 10
  expect(instance.getComputed()).toBe(50); // 5 * 10
});

test("private fields in nested classes", () => {
  class Outer {
    #outerPrivate = "outer";

    getOuter() {
      return this.#outerPrivate;
    }

    createInner() {
      const outerRef = this;

      return class Inner {
        #innerPrivate = "inner";

        getInner() {
          return this.#innerPrivate;
        }

        // Inner class cannot access outer's private fields directly
        tryAccessOuter() {
          // This would fail: return outerRef.#outerPrivate;
          return outerRef.getOuter(); // Must use public method
        }
      };
    }
  }

  const outer = new Outer();
  const InnerClass = outer.createInner();
  const inner = new InnerClass();

  expect(outer.getOuter()).toBe("outer");
  expect(inner.getInner()).toBe("inner");
  expect(inner.tryAccessOuter()).toBe("outer");
});

test("private field access with arrow functions", () => {
  class TestClass {
    #value = 42;

    // Arrow function in class body
    arrowMethod = () => {
      return this.#value;
    };

    // Regular method that returns arrow function
    getArrowFunction() {
      return () => this.#value;
    }

    // Arrow function with parameter
    arrowWithParam = (multiplier) => {
      return this.#value * multiplier;
    };
  }

  const instance = new TestClass();

  expect(instance.arrowMethod()).toBe(42);
  expect(instance.arrowWithParam(2)).toBe(84);

  const arrowFn = instance.getArrowFunction();
  expect(arrowFn()).toBe(42);

  // Test that arrow functions maintain context
  const detachedArrow = instance.arrowMethod;
  expect(detachedArrow()).toBe(42);
});

test("private field brand checking", () => {
  class TestClass {
    #brand = true;

    static isInstance(obj) {
      try {
        // This will throw if obj doesn't have the private field
        return obj.#brand === true;
      } catch {
        return false;
      }
    }
  }

  const instance = new TestClass();
  const other = {};

  expect(TestClass.isInstance(instance)).toBe(true);
  expect(TestClass.isInstance(other)).toBe(false);
  expect(TestClass.isInstance(null)).toBe(false);
  expect(TestClass.isInstance(undefined)).toBe(false);
});
