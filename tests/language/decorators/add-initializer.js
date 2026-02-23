/*---
description: addInitializer on decorator context runs callbacks at the correct time
features: [decorators]
---*/

describe("addInitializer", () => {
  test("class addInitializer runs after class definition", () => {
    let initialized = false;

    const init = (cls, context) => {
      context.addInitializer(() => {
        initialized = true;
      });
    };

    expect(initialized).toBe(false);

    @init
    class C {}

    expect(initialized).toBe(true);
  });

  test("method addInitializer runs during construction", () => {
    let initCalled = false;

    const dec = (method, context) => {
      context.addInitializer(() => {
        initCalled = true;
      });
    };

    class C {
      @dec
      foo() {}
    }

    expect(initCalled).toBe(false);
    const c = new C();
    expect(initCalled).toBe(true);
  });
});
