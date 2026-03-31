/*---
description: Class decorators receive the class and can wrap or replace it
features: [decorators]
---*/

describe("class decorators", () => {
  test("decorator receives class and context", () => {
    let receivedClass;
    let receivedContext;

    const log = (cls, context) => {
      receivedClass = cls;
      receivedContext = context;
    };

    @log
    class MyClass {
      value = 42;
    }

    expect(receivedContext.kind).toBe("class");
    expect(receivedContext.name).toBe("MyClass");
  });

  test("decorator returning undefined keeps original class", () => {
    const noop = (cls, context) => {
      return undefined;
    };

    @noop
    class C {
      greet() {
        return "hello";
      }
    }

    const c = new C();
    expect(c.greet()).toBe("hello");
  });

  test("multiple class decorators applied bottom-up", () => {
    const calls = [];

    const first = (cls, context) => {
      calls.push("first");
    };

    const second = (cls, context) => {
      calls.push("second");
    };

    @first
    @second
    class C {}

    expect(calls[0]).toBe("second");
    expect(calls[1]).toBe("first");
  });

  test("class decorator can wrap with subclass constructor calling super()", () => {
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
      constructor() {
        this.x = 1;
      }
    }

    const c = new C();
    expect(c.x).toBe(1);
    expect(c.wrapped).toBe(true);
  });
});
