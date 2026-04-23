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

describe("class as object property", () => {
  test("anonymous class expression infers name from property key", () => {
    const obj = { MyClass: class {} };
    expect(obj.MyClass.name).toBe("MyClass");
  });
});

describe("static name override", () => {
  test("static getter overrides default .name", () => {
    class Foo { static get name() { return "Custom"; } }
    expect(Foo.name).toBe("Custom");
  });

  test("static field overrides default .name", () => {
    class Bar { static name = "Override"; }
    expect(Bar.name).toBe("Override");
  });

  test("inherited static name getter does not shadow child built-in name", () => {
    class Parent { static get name() { return "ParentCustom"; } }
    class Child extends Parent {}
    expect(Child.name).toBe("Child");
    expect(Parent.name).toBe("ParentCustom");
  });

  test("setter-only static name shadows built-in name with undefined", () => {
    class Foo { static set name(v) {} }
    expect(Foo.name).toBe(undefined);
  });
});
