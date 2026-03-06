/*---
description: Named class expressions bind the name in the inner scope
features: [classes]
---*/

test("named class expression - self reference from method", () => {
  const Foo = class Bar {
    getClass() {
      return Bar;
    }
  };

  const foo = new Foo();
  expect(foo.getClass()).toBe(Foo);
});

test("named class expression - self reference from static method", () => {
  const MyClass = class InternalName {
    static create() {
      return new InternalName();
    }
  };

  const instance = MyClass.create();
  expect(instance instanceof MyClass).toBe(true);
});

test("named class expression - name not visible outside", () => {
  const Foo = class Bar {};
  expect(typeof Bar === "undefined" || typeof Bar === "object").toBe(true);
});
