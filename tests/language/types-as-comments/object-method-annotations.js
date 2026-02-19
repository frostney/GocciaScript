/*---
description: Type annotations on object methods are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("object method parameter types", () => {
  const obj = {
    add(a: number, b: number) {
      return a + b;
    }
  };
  expect(obj.add(1, 2)).toBe(3);
});

test("object method return type", () => {
  const obj = {
    greet(name: string): string {
      return "Hello " + name;
    }
  };
  expect(obj.greet("World")).toBe("Hello World");
});

test("object getter return type", () => {
  const obj = {
    _value: 10,
    get value(): number {
      return this._value;
    }
  };
  expect(obj.value).toBe(10);
});

test("object setter parameter type", () => {
  const obj = {
    _value: 0,
    set value(v: number) {
      this._value = v;
    },
    get value() {
      return this._value;
    }
  };
  obj.value = 42;
  expect(obj.value).toBe(42);
});
