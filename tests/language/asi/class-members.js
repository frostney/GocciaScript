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

  // ES2026 §15.8 / §15.6: AsyncMethod and AsyncGeneratorMethod are
  // `async [no LineTerminator here] ...`, so after a line break `async` is a
  // field that ASI ends before the next element.
  test("async followed by a line break is a field, not a method prefix", async () => {
    class Plain {
      async
      value() {
        return 1;
      }
    }
    const plain = new Plain();
    expect(Object.keys(plain)).toEqual(["async"]);
    expect(plain.async).toBeUndefined();
    expect(plain.value()).toBe(1);

    class Generator {
      async
      *values() {
        yield 1;
      }
    }
    expect(Object.keys(new Generator())).toEqual(["async"]);
    expect([...new Generator().values()]).toEqual([1]);

    class Static {
      static async
      value() {
        return 2;
      }
    }
    expect(Object.keys(Static)).toEqual(["async"]);
    expect(Static.value).toBeUndefined();
    expect(new Static().value()).toBe(2);

    class SameLine {
      async value() {
        return 3;
      }
    }
    expect(await new SameLine().value()).toBe(3);
  });

  test("static field without semicolon", () => {
    class Config {
      static defaultValue = 100
    }
    expect(Config.defaultValue).toBe(100);
  });
});
