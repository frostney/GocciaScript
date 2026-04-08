/*---
description: ASI for class field declarations
features: [automatic-semicolon-insertion]
---*/

describe("ASI class members", () => {
  test("class field with initializer without semicolon", () => {
    class Foo {
      x = 10
      y = 20
    }
    const foo = new Foo();
    expect(foo.x).toBe(10);
    expect(foo.y).toBe(20);
  });

  test("class field without initializer without semicolon", () => {
    class Bar {
      x
      y
    }
    const bar = new Bar();
    expect(bar.x).toBeUndefined();
    expect(bar.y).toBeUndefined();
  });

  test("mixed fields and methods without semicolons on fields", () => {
    class Baz {
      value = 42
      getValue() {
        return this.value
      }
    }
    const baz = new Baz();
    expect(baz.getValue()).toBe(42);
  });

  test("static field without semicolon", () => {
    class Config {
      static defaultValue = 100
    }
    expect(Config.defaultValue).toBe(100);
  });
});
