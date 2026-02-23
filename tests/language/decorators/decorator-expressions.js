/*---
description: Decorator expressions support property access and calls
features: [decorators]
---*/

describe("decorator expressions", () => {
  test("@identifier", () => {
    let called = false;
    const dec = (value, context) => { called = true; };

    class C {
      @dec
      foo() {}
    }

    expect(called).toBe(true);
  });

  test("@obj.property", () => {
    let called = false;
    const decorators = {
      log: (value, context) => { called = true; },
    };

    class C {
      @decorators.log
      foo() {}
    }

    expect(called).toBe(true);
  });

  test("@factory(arg)", () => {
    let receivedArg;

    const withTag = (tag) => {
      return (value, context) => {
        receivedArg = tag;
      };
    };

    class C {
      @withTag("important")
      foo() {}
    }

    expect(receivedArg).toBe("important");
  });

  test("@obj.factory(arg)", () => {
    let receivedArg;

    const decorators = {
      withTag: (tag) => {
        return (value, context) => {
          receivedArg = tag;
        };
      },
    };

    class C {
      @decorators.withTag("test")
      foo() {}
    }

    expect(receivedArg).toBe("test");
  });
});
