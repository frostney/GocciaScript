/*---
description: Private fields and methods work correctly
features: [private-fields, private-methods]
---*/

test("private fields", () => {
  class BankAccount {
    #balance = 0;

    constructor(initialBalance) {
      this.#balance = initialBalance;
    }

    deposit(amount) {
      this.#balance += amount;
    }

    withdraw(amount) {
      if (amount <= this.#balance) {
        this.#balance -= amount;
        return amount;
      }
      return 0;
    }

    getBalance() {
      return this.#balance;
    }
  }

  const account = new BankAccount(100);
  expect(account.getBalance()).toBe(100);

  account.deposit(50);
  expect(account.getBalance()).toBe(150);

  const withdrawn = account.withdraw(30);
  expect(withdrawn).toBe(30);
  expect(account.getBalance()).toBe(120);

  // Private field is not accessible
  expect(account.balance).toBeUndefined();
  expect(account["#balance"]).toBeUndefined();

  const keys = Object.keys(account);
  expect(keys).not.toContain("#balance");
  expect(keys).not.toContain("balance");
});

test("private methods", () => {
  class Calculator {
    #result = 0;

    #validate(value) {
      return typeof value === "number" && !Number.isNaN(value);
    }

    add(value) {
      if (this.#validate(value)) {
        this.#result += value;
      }
      return this;
    }

    multiply(value) {
      if (this.#validate(value)) {
        this.#result *= value;
      }
      return this;
    }

    getResult() {
      return this.#result;
    }
  }

  const calc = new Calculator();
  expect(calc.add(5).multiply(3).getResult()).toBe(15);

  // Invalid operations are ignored
  calc.add("invalid").multiply(2);
  expect(calc.getResult()).toBe(30); // Only valid multiply(2) was applied

  // Private method is not accessible
  expect(calc.validate).toBeUndefined();
});

test("comprehensive private fields with inheritance", () => {
  class Counter {
    #count = 0;
    #maxValue = 100;

    constructor(initialValue) {
      if (initialValue !== undefined) {
        this.#count = initialValue;
      }
    }

    increment() {
      if (this.#count < this.#maxValue) {
        this.#count = this.#count + 1;
      }
      return this;
    }

    getCount() {
      return this.#count;
    }

    #validateCount(value) {
      return value >= 0 && value <= this.#maxValue;
    }

    setCount(value) {
      if (this.#validateCount(value)) {
        this.#count = value;
      }
      return this;
    }
  }

  // Test basic functionality
  const counter = new Counter();
  expect(counter.getCount()).toBe(0);

  counter.increment().increment().increment();
  expect(counter.getCount()).toBe(3);

  counter.setCount(10);
  expect(counter.getCount()).toBe(10);

  // Test private field initialization with constructor
  const counter2 = new Counter(5);
  expect(counter2.getCount()).toBe(5);

  // Test inheritance - private fields are not inherited
  class AdvancedCounter extends Counter {
    #multiplier = 2;

    doubleIncrement() {
      // This should access the parent's public methods
      super.increment();
      super.increment();
      return this;
    }

    getMultiplier() {
      return this.#multiplier;
    }
  }

  const advCounter = new AdvancedCounter(1);
  expect(advCounter.getCount()).toBe(1);

  advCounter.doubleIncrement();
  expect(advCounter.getCount()).toBe(3);

  expect(advCounter.getMultiplier()).toBe(2);
});

test("private fields support shadowing across inheritance hierarchies", () => {
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
    #basePrivate = "shadowed";
    #derivedPrivate = "derived";

    getShadowedPrivate() {
      return this.#basePrivate;
    }

    getDerivedPrivate() {
      return this.#derivedPrivate;
    }
  }

  const instance = new Derived();

  expect(instance.getBasePrivate()).toBe("base");
  expect(instance.getShadowedPrivate()).toBe("shadowed");
  expect(instance.getDerivedPrivate()).toBe("derived");

  instance.setBasePrivate("modified");
  expect(instance.getBasePrivate()).toBe("modified");
  expect(instance.getShadowedPrivate()).toBe("shadowed");
});

test("private field initialization order is stable", () => {
  const initOrder = [];

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
    #extra;

    constructor(extra) {
      initOrder.push("constructor");
      this.#extra = extra;
    }

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

  expect(instance.getValues()).toEqual({
    first: 1,
    second: 2,
    computed: 3,
    extra: 10,
  });
  expect(initOrder).toEqual(["first", "second", "constructor"]);
});

test("nested classes cannot directly access outer private fields", () => {
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

        tryAccessOuter() {
          return outerRef.getOuter();
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

test("private field brand checks reject non-instances", () => {
  class TestClass {
    #brand = true;

    static isInstance(obj) {
      try {
        return obj.#brand === true;
      } catch (e) {
        return false;
      }
    }
  }

  const instance = new TestClass();

  expect(TestClass.isInstance(instance)).toBe(true);
  expect(TestClass.isInstance({})).toBe(false);
  expect(TestClass.isInstance(null)).toBe(false);
  expect(TestClass.isInstance(undefined)).toBe(false);
});

test("private fields on replacement receivers preserve initialization order", () => {
  const order = [];
  let derivedPrototype;

  class Base {
    constructor() {
      order.length = 0;
      return Object.create(derivedPrototype);
    }
  }

  class Derived extends Base {
    a = order.push("a");
    #b = order.push("b");
    c = order.push("c");

    readOrder() {
      return order.join(",");
    }
  }

  derivedPrototype = Derived.prototype;

  const instance = new Derived();
  expect(instance.readOrder()).toBe("a,b,c");
});

test("private brand checks reject unbranded replacement receivers", () => {
  let derivedPrototype;

  class Base {
    constructor() {
      return Object.create(derivedPrototype);
    }
  }

  class Derived extends Base {
    #value = 1;

    static writeTo(obj, value) {
      obj.#value = value;
      return obj.#value;
    }

    static readFrom(obj) {
      return obj.#value;
    }

    read() {
      return this.#value;
    }

    write(value) {
      this.#value = value;
      return this.#value;
    }
  }

  derivedPrototype = Derived.prototype;

  const instance = new Derived();
  expect(instance.read()).toBe(1);
  expect(instance.write(2)).toBe(2);
  expect(Derived.readFrom(instance)).toBe(2);
  expect(Derived.writeTo(instance, 3)).toBe(3);
  expect(() => Derived.readFrom({})).toThrow(TypeError);
  expect(() => Derived.writeTo({})).toThrow(TypeError);
});

test("private fields on replacement receivers use distinct class brands", () => {
  const First = (() => {
    let derivedPrototype;

    class Base {
      constructor() {
        return Object.create(derivedPrototype);
      }
    }

    class SameName extends Base {
      #value = "first";

      read() {
        return this.#value;
      }

      static readFrom(obj) {
        return obj.#value;
      }
    }

    derivedPrototype = SameName.prototype;
    return SameName;
  })();

  const Second = (() => {
    let derivedPrototype;

    class Base {
      constructor() {
        return Object.create(derivedPrototype);
      }
    }

    class SameName extends Base {
      #value = "second";

      read() {
        return this.#value;
      }

      static readFrom(obj) {
        return obj.#value;
      }
    }

    derivedPrototype = SameName.prototype;
    return SameName;
  })();

  const first = new First();
  const second = new Second();

  expect(first.read()).toBe("first");
  expect(second.read()).toBe("second");
  expect(First.readFrom(first)).toBe("first");
  expect(Second.readFrom(second)).toBe("second");
  expect(() => First.readFrom(second)).toThrow(TypeError);
});
