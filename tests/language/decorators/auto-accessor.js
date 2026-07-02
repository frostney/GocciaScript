/*---
description: Auto-accessor class elements create getter/setter pairs with a backing field
features: [decorators, auto-accessor]
---*/

describe("auto-accessor", () => {
  test("basic auto-accessor with initializer", () => {
    class C {
      accessor x = 42;
    }

    const c = new C();
    expect(c.x).toBe(42);
  });

  test("auto-accessor can be set", () => {
    class C {
      accessor x = 0;
    }

    const c = new C();
    c.x = 100;
    expect(c.x).toBe(100);
  });

  test("auto-accessor without initializer", () => {
    class C {
      accessor x;
    }

    const c = new C();
    expect(c.x).toBe(undefined);
  });

  test("private auto-accessor uses private storage", () => {
    class C {
      accessor #x;

      hasX() {
        return #x in this;
      }

      read() {
        return this.#x;
      }

      write(value) {
        this.#x = value;
      }
    }

    const c = new C();
    expect(c.hasX()).toBe(true);
    expect(c.read()).toBe(undefined);
    c.write(42);
    expect(c.read()).toBe(42);
    expect(Object.getOwnPropertyNames(c).length).toBe(0);
  });

  test("computed auto-accessor uses resolved property key", () => {
    const key = "x";

    class C {
      accessor [key] = 1;
    }

    const c = new C();
    expect(c.x).toBe(1);
    c.x = 2;
    expect(c.x).toBe(2);
  });

  test("symbol computed auto-accessor uses resolved property key", () => {
    const key = Symbol("x");

    class C {
      accessor [key] = 1;
    }

    const c = new C();
    expect(c[key]).toBe(1);
    c[key] = 2;
    expect(c[key]).toBe(2);
  });
});
