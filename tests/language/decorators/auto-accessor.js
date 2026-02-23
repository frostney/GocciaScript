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
});
