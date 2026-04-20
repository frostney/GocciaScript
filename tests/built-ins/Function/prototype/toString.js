/*---
description: Function.prototype.toString
features: [Function]
---*/

describe("Function.prototype.toString", () => {
  test("native function returns NativeFunction string", () => {
    const result = console.log.toString();
    expect(result).toBe("function log() { [native code] }");
  });

  test("arrow function returns source text", () => {
    const add = (a, b) => a + b;
    expect(add.toString()).toBe("(a, b) => a + b");
  });

  test("arrow function with block body returns source text", () => {
    const fn = (x) => { return x * 2; };
    expect(fn.toString()).toBe("(x) => { return x * 2; }");
  });

  test("single-parameter arrow function returns source text", () => {
    const inc = x => x + 1;
    expect(inc.toString()).toBe("x => x + 1");
  });

  test("multiline arrow function preserves newlines", () => {
    const fn = (a, b) => {
      const sum = a + b;
      return sum;
    };
    const str = fn.toString();
    expect(str.includes("\n")).toBe(true);
    expect(str.startsWith("(a, b) => {")).toBe(true);
    expect(str.endsWith("}")).toBe(true);
  });

  test("bound function returns NativeFunction string", () => {
    const fn = (a, b) => a + b;
    const bound = fn.bind(null, 1);
    const re = /^function\s.*\{\s*\[native code\]\s*\}$/;
    expect(re.test(bound.toString())).toBe(true);
  });

  test("throws TypeError on non-function this", () => {
    const toString = console.log.toString;
    expect(() => toString.call({})).toThrow(TypeError);
    expect(() => toString.call(42)).toThrow(TypeError);
    expect(() => toString.call("str")).toThrow(TypeError);
  });

  test("arrow method on object returns source text", () => {
    const obj = {
      greet: (name) => "hello " + name
    };
    expect(obj.greet.toString()).toBe('(name) => "hello " + name');
  });

  test("object shorthand method returns source text", () => {
    const obj = { add(a, b) { return a + b; } };
    const str = obj.add.toString();
    expect(str.startsWith("add(a, b)")).toBe(true);
    expect(str.includes("return a + b")).toBe(true);
  });

  test("class method returns source text", () => {
    class Calculator {
      multiply(a, b) { return a * b; }
    }
    const calc = new Calculator();
    const str = calc.multiply.toString();
    expect(str.startsWith("multiply(a, b)")).toBe(true);
    expect(str.includes("return a * b")).toBe(true);
  });

  test("class static method returns source text", () => {
    class MathUtils {
      static double(x) { return x * 2; }
    }
    const str = MathUtils.double.toString();
    expect(str.includes("double(x)")).toBe(true);
    expect(str.includes("return x * 2")).toBe(true);
  });

  test("object getter returns source text", () => {
    const obj = { get name() { return "hello"; } };
    const desc = Object.getOwnPropertyDescriptor(obj, "name");
    const str = desc.get.toString();
    expect(str.includes("get")).toBe(true);
    expect(str.includes("name()")).toBe(true);
    expect(str.includes("return")).toBe(true);
  });

  test("object setter returns source text", () => {
    const obj = { set value(v) { this._v = v; } };
    const desc = Object.getOwnPropertyDescriptor(obj, "value");
    const str = desc.set.toString();
    expect(str.includes("set")).toBe(true);
    expect(str.includes("value(v)")).toBe(true);
    expect(str.includes("this._v = v")).toBe(true);
  });

  test("class getter returns source text", () => {
    class Foo {
      get bar() { return 42; }
    }
    const desc = Object.getOwnPropertyDescriptor(Foo.prototype, "bar");
    const str = desc.get.toString();
    expect(str.includes("get")).toBe(true);
    expect(str.includes("bar()")).toBe(true);
    expect(str.includes("return 42")).toBe(true);
  });

  test("class setter returns source text", () => {
    class Foo {
      set bar(v) { this._bar = v; }
    }
    const desc = Object.getOwnPropertyDescriptor(Foo.prototype, "bar");
    const str = desc.set.toString();
    expect(str.includes("set")).toBe(true);
    expect(str.includes("bar(v)")).toBe(true);
    expect(str.includes("this._bar = v")).toBe(true);
  });

  test("async arrow function includes async prefix", () => {
    const fn = async (x) => x + 1;
    const str = fn.toString();
    expect(str.includes("async")).toBe(true);
    expect(str.includes("=>")).toBe(true);
    expect(str.includes("x + 1")).toBe(true);
  });

  test("async object method includes async prefix", () => {
    const obj = { async fetch(url) { return url; } };
    const str = obj.fetch.toString();
    expect(str.includes("async")).toBe(true);
    expect(str.includes("fetch(url)")).toBe(true);
    expect(str.includes("return url")).toBe(true);
  });
});
