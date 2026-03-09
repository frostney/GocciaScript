/*---
description: Class expressions extending dynamic values with super() calls
features: [classes]
---*/

describe("class expression extends dynamic value", () => {
  test("class expression extends parameter with constructor calling super()", () => {
    const makeWrapper = (Base) => {
      return class extends Base {
        constructor() {
          super();
          this.wrapped = true;
        }
      };
    };

    class Original {
      constructor() { this.x = 1; }
    }
    const Wrapped = makeWrapper(Original);
    const w = new Wrapped();
    expect(w.x).toBe(1);
    expect(w.wrapped).toBe(true);
    expect(w instanceof Wrapped).toBe(true);
    expect(w instanceof Original).toBe(true);
  });

  test("class expression extends parameter with spread args in super()", () => {
    const makeWrapper = (Base) => {
      return class extends Base {
        constructor(...args) {
          super(...args);
          this.wrapped = true;
        }
      };
    };

    class Original {
      constructor(a, b) { this.a = a; this.b = b; }
    }
    const Wrapped = makeWrapper(Original);
    const w = new Wrapped(10, 20);
    expect(w.a).toBe(10);
    expect(w.b).toBe(20);
    expect(w.wrapped).toBe(true);
  });

  test("class expression extends parameter without constructor (implicit super)", () => {
    const makeWrapper = (Base) => {
      return class extends Base {};
    };

    class Original {
      constructor() { this.x = 42; }
    }
    const Wrapped = makeWrapper(Original);
    const w = new Wrapped();
    expect(w.x).toBe(42);
    expect(w instanceof Original).toBe(true);
  });

  test("class expression extends parameter with methods", () => {
    const addGreeting = (Base) => {
      return class extends Base {
        constructor(name) {
          super(name);
        }
        greet() { return "Hello, " + this.name; }
      };
    };

    class Person {
      constructor(name) { this.name = name; }
    }
    const Greeter = addGreeting(Person);
    const g = new Greeter("Alice");
    expect(g.name).toBe("Alice");
    expect(g.greet()).toBe("Hello, Alice");
  });

  test("class decorator wrapping with constructor and super()", () => {
    const wrap = (cls, context) => {
      return class extends cls {
        constructor(...args) {
          super(...args);
          this.wrapped = true;
        }
      };
    };

    @wrap
    class C {
      constructor() { this.x = 1; }
    }
    const c = new C();
    expect(c.x).toBe(1);
    expect(c.wrapped).toBe(true);
  });
});
