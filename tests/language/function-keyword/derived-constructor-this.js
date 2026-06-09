/*---
description: Function bodies created in derived constructors do not inherit the derived this guard
features: [compat-function, class-inheritance]
---*/

test("function declaration inside derived constructor can read its own this before super", () => {
  class Base {}

  class Derived extends Base {
    constructor() {
      function nested() {
        return this;
      }

      const beforeSuper = nested();
      super();
      this.beforeSuper = beforeSuper;
    }
  }

  expect(new Derived().beforeSuper).toBeUndefined();
});

test("function expression inside derived constructor can read its own this before super", () => {
  class Base {}

  class Derived extends Base {
    constructor() {
      const nested = function() {
        return this;
      };

      const beforeSuper = nested();
      super();
      this.beforeSuper = beforeSuper;
    }
  }

  expect(new Derived().beforeSuper).toBeUndefined();
});
