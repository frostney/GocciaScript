/*---
description: Class.name property returns the class name per ES spec
features: [classes, Function.name]
---*/

describe("class declaration name", () => {
  test("class declaration has .name matching identifier", () => {
    class MyClass {}
    expect(MyClass.name).toBe("MyClass");
  });

  test("subclass declaration has .name matching identifier", () => {
    class Base {}
    class Child extends Base {}
    expect(Child.name).toBe("Child");
  });
});

describe("class expression name", () => {
  test("anonymous class expression infers name from variable", () => {
    const Foo = class {};
    expect(Foo.name).toBe("Foo");
  });

  test("named class expression uses its own name", () => {
    const Bar = class InternalName {};
    expect(Bar.name).toBe("InternalName");
  });

  test("let-assigned anonymous class expression infers name", () => {
    let Baz = class {};
    expect(Baz.name).toBe("Baz");
  });

  test("unbound anonymous class expression has empty name", () => {
    expect((class {}).name).toBe("");
  });
});
