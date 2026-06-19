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

  test("@obj.default accepts keyword property names", () => {
    let called = false;
    const decorators = {
      default: (value, context) => { called = true; },
    };

    class C {
      @decorators.default
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

  test("class expression decorators", () => {
    const calls = [];
    const dec = (value, context) => {
      calls.push(context.kind);
      value.decorated = true;
    };
    const decorators = {
      log: dec,
    };
    const withTag = (tag) => {
      return (value, context) => {
        value.tag = tag;
      };
    };

    const A = @dec class {};
    const B = @decorators.log class Named {};
    const C = @withTag("class-expression") class {};
    const D = @(dec) class {};

    expect(A.decorated).toBe(true);
    expect(B.decorated).toBe(true);
    expect(C.tag).toBe("class-expression");
    expect(D.decorated).toBe(true);
    expect(calls[0]).toBe("class");
    expect(calls[1]).toBe("class");
    expect(calls[2]).toBe("class");
  });
});
