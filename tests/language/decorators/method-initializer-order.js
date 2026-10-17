/*---
description: Initializers that method decorators add run on each instance before its fields
features: [decorators]
---*/

describe("method decorator addInitializer order", () => {
  test("public method and getter initializers run before the fields", () => {
    const log = [];
    const dec = (method, context) => {
      context.addInitializer(({
        init() {
          log.push(`init ${context.name} ${typeof this.field}`);
        },
      }).init);
    };

    class C {
      field = (log.push("field"), 1);
      @dec foo() {}
      @dec get bar() { return 1; }
    }

    new C();
    expect(log).toEqual(["init foo undefined", "init bar undefined", "field"]);
  });

  test("private method initializers run for each instance before its fields", () => {
    const log = [];
    const dec = (value, context) => {
      context.addInitializer(({
        init() {
          log.push(`init ${typeof this.field}`);
        },
      }).init);
    };

    class A {
      field = (log.push("field"), 1);
      @dec #m() {}
    }

    new A();
    new A();
    expect(log).toEqual(["init undefined", "field", "init undefined", "field"]);
  });
});
