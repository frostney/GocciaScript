/*---
description: Static methods participate in inheritance and super dispatch
features: [static-methods, class-inheritance]
---*/

test("static methods can be inherited and overridden", () => {
  class Parent {
    static parentMethod() {
      return "parent";
    }
  }

  class Child extends Parent {
    static childMethod() {
      return "child";
    }

    static parentMethod() {
      return super.parentMethod() + " overridden";
    }
  }

  expect(Child.parentMethod()).toBe("parent overridden");
  expect(Child.childMethod()).toBe("child");
});
