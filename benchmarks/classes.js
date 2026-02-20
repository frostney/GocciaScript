/*---
description: Class operation benchmarks
---*/

suite("class instantiation", () => {
  bench("simple class new", () => {
    class Point {
      constructor(x, y) {
        this.x = x;
        this.y = y;
      }
    }
    const p = new Point(1, 2);
  });

  bench("class with defaults", () => {
    class Config {
      constructor(opts) {
        this.host = opts.host;
        this.port = opts.port;
        this.debug = false;
        this.retries = 3;
      }
    }
    const c = new Config({ host: "localhost", port: 8080 });
  });

  bench("50 instances via Array.from", () => {
    class Item {
      constructor(id) {
        this.id = id;
        this.active = true;
      }
    }
    const items = Array.from({ length: 50 }, (_, i) => new Item(i));
  });
});

suite("method dispatch", () => {
  bench("instance method call", () => {
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
  });

  bench("static method call", () => {
    class MathHelper {
      static square(x) { return x * x; }
      static cube(x) { return x * x * x; }
    }
    const a = MathHelper.square(7);
    const b = MathHelper.cube(3);
  });
});

suite("inheritance", () => {
  bench("single-level inheritance", () => {
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
  });

  bench("two-level inheritance", () => {
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
  });
});

suite("private fields", () => {
  bench("private field access", () => {
    class Wallet {
      #balance = 0;
      deposit(amount) { this.#balance = this.#balance + amount; }
      get balance() { return this.#balance; }
    }
    const w = new Wallet();
    w.deposit(100);
    w.deposit(50);
    const b = w.balance;
  });

  bench("private methods", () => {
    class Validator {
      #value;
      constructor(v) { this.#value = v; }
      #isPositive() { return this.#value > 0; }
      validate() { return this.#isPositive(); }
    }
    const v = new Validator(42);
    const ok = v.validate();
  });
});

suite("getters and setters", () => {
  bench("getter/setter access", () => {
    class Temperature {
      constructor(c) { this.celsius = c; }
      get fahrenheit() { return this.celsius * 9 / 5 + 32; }
      set fahrenheit(f) { this.celsius = (f - 32) * 5 / 9; }
    }
    const t = new Temperature(100);
    const f = t.fahrenheit;
    t.fahrenheit = 72;
  });
});

suite("static getters and setters", () => {
  bench("static getter read", () => {
    class Config {
      static get defaultPort() { return 8080; }
      static get defaultHost() { return "localhost"; }
    }
    const p = Config.defaultPort;
    const h = Config.defaultHost;
  });

  bench("static getter/setter pair", () => {
    class Registry {
      static _count = 0;
      static get count() { return Registry._count; }
      static set count(v) { Registry._count = v; }
    }
    Registry.count = 10;
    const c = Registry.count;
    Registry.count = c + 1;
  });

  bench("inherited static getter", () => {
    class Base {
      static get kind() { return "base"; }
    }
    class Child extends Base {}
    class GrandChild extends Child {}
    const a = Child.kind;
    const b = GrandChild.kind;
  });

  bench("inherited static setter", () => {
    class Base {
      static _val = 0;
      static get val() { return Base._val; }
      static set val(v) { Base._val = v; }
    }
    class Child extends Base {}
    Child.val = 42;
    const v = Child.val;
  });

  bench("inherited static getter with this binding", () => {
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
  });
});
