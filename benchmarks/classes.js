/*---
description: Class operation benchmarks
---*/

suite("class instantiation", () => {
  bench("simple class new", {
    run: () => {
      class Point {
        constructor(x, y) {
          this.x = x;
          this.y = y;
        }
      }
      const p = new Point(1, 2);
    },
  });

  bench("class with defaults", {
    run: () => {
      class Config {
        constructor(opts) {
          this.host = opts.host;
          this.port = opts.port;
          this.debug = false;
          this.retries = 3;
        }
      }
      const c = new Config({ host: "localhost", port: 8080 });
    },
  });

  bench("50 instances via Array.from", {
    run: () => {
      class Item {
        constructor(id) {
          this.id = id;
          this.active = true;
        }
      }
      const items = Array.from({ length: 50 }, (_, i) => new Item(i));
    },
  });
});

suite("method dispatch", () => {
  bench("instance method call", {
    run: () => {
      class Counter {
        constructor() { this.count = 0; }
        increment() { this.count = this.count + 1; }
        get value() { return this.count; }
      }
      const c = new Counter();
      c.increment();
      c.increment();
      c.increment();
      const v = c.value;
    },
  });

  bench("static method call", {
    run: () => {
      class MathHelper {
        static square(x) { return x * x; }
        static cube(x) { return x * x * x; }
      }
      const a = MathHelper.square(7);
      const b = MathHelper.cube(3);
    },
  });
});

suite("inheritance", () => {
  bench("single-level inheritance", {
    run: () => {
      class Shape {
        constructor(name) { this.name = name; }
        describe() { return this.name; }
      }
      class Circle extends Shape {
        constructor(radius) {
          super("circle");
          this.radius = radius;
        }
        area() { return 3.14159 * this.radius * this.radius; }
      }
      const c = new Circle(5);
      const a = c.area();
      const d = c.describe();
    },
  });

  bench("two-level inheritance", {
    run: () => {
      class Animal {
        constructor(name) { this.name = name; }
        speak() { return this.name; }
      }
      class Dog extends Animal {
        constructor(name, breed) {
          super(name);
          this.breed = breed;
        }
      }
      class Poodle extends Dog {
        constructor(name) {
          super(name, "poodle");
          this.groomed = false;
        }
      }
      const p = new Poodle("Max");
      const s = p.speak();
    },
  });
});

suite("private fields", () => {
  bench("private field access", {
    run: () => {
      class Wallet {
        #balance = 0;
        deposit(amount) { this.#balance = this.#balance + amount; }
        get balance() { return this.#balance; }
      }
      const w = new Wallet();
      w.deposit(100);
      w.deposit(50);
      const b = w.balance;
    },
  });

  bench("private methods", {
    run: () => {
      class Validator {
        #value;
        constructor(v) { this.#value = v; }
        #isPositive() { return this.#value > 0; }
        validate() { return this.#isPositive(); }
      }
      const v = new Validator(42);
      const ok = v.validate();
    },
  });
});

suite("getters and setters", () => {
  bench("getter/setter access", {
    run: () => {
      class Temperature {
        constructor(c) { this.celsius = c; }
        get fahrenheit() { return this.celsius * 9 / 5 + 32; }
        set fahrenheit(f) { this.celsius = (f - 32) * 5 / 9; }
      }
      const t = new Temperature(100);
      const f = t.fahrenheit;
      t.fahrenheit = 72;
    },
  });
});

suite("decorators: class", () => {
  bench("class decorator (identity)", {
    run: () => {
      const noop = (cls, context) => cls;

      @noop
      class C {
        value = 1;
      }
      const c = new C();
    },
  });

  bench("class decorator (wrapping)", {
    run: () => {
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
    },
  });
});

suite("decorators: method", () => {
  bench("identity method decorator", {
    run: () => {
      const noop = (method, context) => method;

      class C {
        @noop
        greet() { return "hello"; }
      }
      const c = new C();
      const v = c.greet();
    },
  });

  bench("wrapping method decorator", {
    run: () => {
      const logged = (method, context) => {
        return (...args) => method(...args);
      };

      class C {
        @logged
        compute(x) { return x * 2; }
      }
      const c = new C();
      const v = c.compute(5);
    },
  });

  bench("stacked method decorators (x3)", {
    run: () => {
      const d1 = (method, context) => method;
      const d2 = (method, context) => method;
      const d3 = (method, context) => method;

      class C {
        @d1 @d2 @d3
        run() { return 42; }
      }
      const c = new C();
      const v = c.run();
    },
  });
});

suite("decorators: field", () => {
  bench("identity field decorator", {
    run: () => {
      const noop = (value, context) => {};

      class C {
        @noop
        x = 10;
      }
      const c = new C();
    },
  });

  bench("field initializer decorator", {
    run: () => {
      const double = (value, context) => {
        return (initialValue) => initialValue * 2;
      };

      class C {
        @double
        x = 5;
      }
      const c = new C();
    },
  });
});

suite("decorators: getter/setter", () => {
  bench("getter decorator (identity)", {
    run: () => {
      const noop = (getter, context) => getter;

      class C {
        #val = 42;
        @noop
        get value() { return this.#val; }
      }
      const c = new C();
      const v = c.value;
    },
  });

  bench("setter decorator (identity)", {
    run: () => {
      const noop = (setter, context) => setter;

      class C {
        #val = 0;
        get value() { return this.#val; }
        @noop
        set value(v) { this.#val = v; }
      }
      const c = new C();
      c.value = 99;
      const v = c.value;
    },
  });
});

suite("decorators: static", () => {
  bench("static method decorator", {
    run: () => {
      const noop = (method, context) => method;

      class C {
        @noop
        static compute(x) { return x + 1; }
      }
      const v = C.compute(10);
    },
  });

  bench("static field decorator", {
    run: () => {
      const noop = (value, context) => {};

      class C {
        @noop
        static label = "hello";
      }
      const v = C.label;
    },
  });
});

suite("decorators: private", () => {
  bench("private method decorator", {
    run: () => {
      const noop = (method, context) => method;

      class C {
        @noop
        #compute(x) { return x * 3; }
        run(x) { return this.#compute(x); }
      }
      const c = new C();
      const v = c.run(7);
    },
  });

  bench("private field decorator", {
    run: () => {
      const noop = (value, context) => {};

      class C {
        @noop
        #secret = 42;
        get secret() { return this.#secret; }
      }
      const c = new C();
      const v = c.secret;
    },
  });
});

suite("decorators: auto-accessor", () => {
  bench("plain auto-accessor (no decorator)", {
    run: () => {
      class C {
        accessor name = "world";
      }
      const c = new C();
      const v = c.name;
      c.name = "updated";
    },
  });

  bench("auto-accessor with decorator", {
    run: () => {
      const noop = (value, context) => value;

      class C {
        @noop
        accessor count = 0;
      }
      const c = new C();
      const v = c.count;
      c.count = 5;
    },
  });
});

suite("decorators: metadata", () => {
  bench("decorator writing metadata", {
    run: () => {
      const meta = (key, val) => (value, context) => {
        context.metadata[key] = val;
      };

      @meta("kind", "entity")
      class C {
        @meta("role", "id")
        id = 1;
      }
      const m = C[Symbol.metadata];
    },
  });
});

suite("static getters and setters", () => {
  bench("static getter read", {
    run: () => {
      class Config {
        static get defaultPort() { return 8080; }
        static get defaultHost() { return "localhost"; }
      }
      const p = Config.defaultPort;
      const h = Config.defaultHost;
    },
  });

  bench("static getter/setter pair", {
    run: () => {
      class Registry {
        static _count = 0;
        static get count() { return Registry._count; }
        static set count(v) { Registry._count = v; }
      }
      Registry.count = 10;
      const c = Registry.count;
      Registry.count = c + 1;
    },
  });

  bench("inherited static getter", {
    run: () => {
      class Base {
        static get kind() { return "base"; }
      }
      class Child extends Base {}
      class GrandChild extends Child {}
      const a = Child.kind;
      const b = GrandChild.kind;
    },
  });

  bench("inherited static setter", {
    run: () => {
      class Base {
        static _val = 0;
        static get val() { return Base._val; }
        static set val(v) { Base._val = v; }
      }
      class Child extends Base {}
      Child.val = 42;
      const v = Child.val;
    },
  });

  bench("inherited static getter with this binding", {
    run: () => {
      class Base {
        static _label = "Base";
        static get label() { return this._label; }
        static set label(v) { this._label = v; }
      }
      class Child extends Base {
        static _label = "Child";
      }
      const baseLabel = Base.label;
      const childLabel = Child.label;
      Child.label = "Updated";
      const updated = Child.label;
    },
  });
});
