describe("Number constructor", () => {
  test("new Number() creates wrapper object", () => {
    const n = new Number(42);
    expect(typeof n).toBe("object");
    expect(n.valueOf()).toBe(42);
  });

  test("new Number() has number methods", () => {
    const n = new Number(3.14159);
    expect(n.toFixed(2)).toBe("3.14");
  });

  test("Number() as function returns primitive", () => {
    const n = Number("42");
    expect(typeof n).toBe("number");
    expect(n).toBe(42);
  });

  test("new Number() instanceof Number", () => {
    const n = new Number(42);
    expect(n instanceof Number).toBe(true);
  });

  test("Number(obj) invokes user valueOf() (ES2026 §7.1.4 ToNumber)", () => {
    class Foo {
      valueOf() { return 42; }
    }
    expect(Number(new Foo())).toBe(42);
  });

  test("Number(obj) prefers valueOf() over toString() (number hint)", () => {
    const obj = {
      valueOf() { return 7; },
      toString() { return "99"; },
    };
    expect(Number(obj)).toBe(7);
  });

  test("Number(obj) falls back to toString() when valueOf() returns non-primitive", () => {
    const obj = {
      valueOf() { return {}; },
      toString() { return "123"; },
    };
    expect(Number(obj)).toBe(123);
  });

  test("Number(obj) throws TypeError when neither method returns a primitive", () => {
    const obj = {
      valueOf() { return {}; },
      toString() { return {}; },
    };
    expect(() => Number(obj)).toThrow(TypeError);
  });

  test("new Number(obj) invokes user valueOf() and wraps the result", () => {
    class Foo {
      valueOf() { return 7; }
    }
    const n = new Number(new Foo());
    expect(typeof n).toBe("object");
    expect(n.valueOf()).toBe(7);
  });

  test("Number(numberWrapper) unwraps via valueOf()", () => {
    const wrapped = new Number(55);
    expect(Number(wrapped)).toBe(55);
  });

  test("Number(Symbol()) throws TypeError", () => {
    expect(() => Number(Symbol("x"))).toThrow(TypeError);
  });

  test("Number(BigInt) explicitly converts while implicit coercion throws", () => {
    expect(Number(42n)).toBe(42);
    expect(new Number(42n).valueOf()).toBe(42);
    expect(() => 42n * 1).toThrow(TypeError);
  });

  test("Number(array) coerces via ToPrimitive, not element-wise", () => {
    expect(Number([])).toBe(0);
    expect(Number([42])).toBe(42);
    expect(Number([1, 2])).toBeNaN();
    expect(Number([true])).toBeNaN();
    expect(Number([false])).toBeNaN();
    expect(Number([null])).toBe(0);
    expect(Number([undefined])).toBe(0);
  });
});
