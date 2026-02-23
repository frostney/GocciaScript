/*---
description: Decorator context object has correct properties for each element kind
features: [decorators]
---*/

describe("decorator context properties", () => {
  test("method context has correct properties", () => {
    let ctx;
    const capture = (v, c) => { ctx = c; };

    class C {
      @capture
      foo() {}
    }

    expect(ctx.kind).toBe("method");
    expect(ctx.name).toBe("foo");
    expect(ctx.static).toBe(false);
    expect(ctx.private).toBe(false);
    expect(typeof ctx.addInitializer).toBe("function");
    expect(typeof ctx.metadata).toBe("object");
  });

  test("static method context", () => {
    let ctx;
    const capture = (v, c) => { ctx = c; };

    class C {
      @capture
      static bar() {}
    }

    expect(ctx.kind).toBe("method");
    expect(ctx.name).toBe("bar");
    expect(ctx.static).toBe(true);
    expect(ctx.private).toBe(false);
  });

  test("field context has correct properties", () => {
    let ctx;
    const capture = (v, c) => { ctx = c; };

    class C {
      @capture
      x = 1;
    }

    expect(ctx.kind).toBe("field");
    expect(ctx.name).toBe("x");
    expect(ctx.static).toBe(false);
    expect(ctx.private).toBe(false);
  });

  test("static field context", () => {
    let ctx;
    const capture = (v, c) => { ctx = c; };

    class C {
      @capture
      static y = 2;
    }

    expect(ctx.kind).toBe("field");
    expect(ctx.name).toBe("y");
    expect(ctx.static).toBe(true);
  });

  test("class decorator context has correct properties", () => {
    let ctx;
    const capture = (cls, c) => { ctx = c; };

    @capture
    class MyClass {}

    expect(ctx.kind).toBe("class");
    expect(ctx.name).toBe("MyClass");
    expect(typeof ctx.addInitializer).toBe("function");
    expect(typeof ctx.metadata).toBe("object");
  });
});
