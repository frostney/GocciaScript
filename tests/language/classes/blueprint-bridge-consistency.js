/*---
description: Blueprint bridge produces consistent class values for instanceof and construction
features: [classes, private-fields, static-methods, getters-setters]
---*/

describe("bridge consistency between value unwrap and construction", () => {
  test("class passed as value then constructed preserves instanceof", () => {
    class MyClass {
      constructor(x) {
        this.x = x;
      }
    }

    const ClassRef = MyClass;
    const obj = new ClassRef(42);
    expect(obj instanceof MyClass).toBe(true);
    expect(obj instanceof ClassRef).toBe(true);
    expect(obj.x).toBe(42);
  });

  test("class with static methods survives bridging", () => {
    class Counter {
      static count = 0;

      static increment() {
        Counter.count = Counter.count + 1;
        return Counter.count;
      }

      constructor() {
        this.id = Counter.count;
      }
    }

    expect(Counter.increment()).toBe(1);
    const c = new Counter();
    expect(c.id).toBe(1);
    expect(c instanceof Counter).toBe(true);
  });

  test("class with getters and setters survives bridging", () => {
    class Box {
      #value;

      constructor(v) {
        this.#value = v;
      }

      get contents() {
        return this.#value;
      }

      set contents(v) {
        this.#value = v;
      }
    }

    const b = new Box(10);
    expect(b.contents).toBe(10);
    b.contents = 20;
    expect(b.contents).toBe(20);
    expect(b instanceof Box).toBe(true);
  });

  test("class with private fields survives bridging", () => {
    class Secret {
      #data;

      constructor(d) {
        this.#data = d;
      }

      reveal() {
        return this.#data;
      }
    }

    const s = new Secret("hidden");
    expect(s.reveal()).toBe("hidden");
    expect(s instanceof Secret).toBe(true);
  });

  test("subclass instanceof checks both parent and child", () => {
    class Parent {
      constructor() {
        this.kind = "parent";
      }
    }

    class Child extends Parent {
      constructor() {
        super();
        this.kind = "child";
      }
    }

    const c = new Child();
    expect(c instanceof Child).toBe(true);
    expect(c instanceof Parent).toBe(true);
    expect(c.kind).toBe("child");
  });
});
