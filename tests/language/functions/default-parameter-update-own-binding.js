/*---
description: A default parameter initializer that updates its own parameter reads it in its temporal dead zone, and is skipped when an argument is passed
features: [default-parameters, update-expressions, temporal-dead-zone]
---*/

describe("default parameter initializer that updates its own parameter", () => {
  test("postfix ++ and -- throw ReferenceError without an argument", () => {
    expect(() => ((a = a++) => a)()).toThrow(ReferenceError);
    expect(() => ((a = a--) => a)()).toThrow(ReferenceError);
    expect(() => ((a = a++) => a)(undefined)).toThrow(ReferenceError);
  });

  test("prefix ++ and -- throw ReferenceError without an argument", () => {
    expect(() => ((a = ++a) => a)()).toThrow(ReferenceError);
    expect(() => ((a = --a) => a)()).toThrow(ReferenceError);
  });

  test("an argument skips the initializer", () => {
    expect(((a = a++) => a)(5)).toBe(5);
    expect(((a = a--) => a)(5)).toBe(5);
    expect(((a = ++a) => a)(5)).toBe(5);
    expect(((a = --a) => a)("x")).toBe("x");
  });

  test("a later parameter in the list", () => {
    expect(() => ((b, a = a++) => a)(1)).toThrow(ReferenceError);
    expect(((b, a = a++) => [b, a])(1, 2)).toEqual([1, 2]);
  });

  test("the update inside a comma or conditional expression", () => {
    expect(() => ((a = (0, a++)) => a)()).toThrow(ReferenceError);
    expect(() => ((a = true ? a-- : 0) => a)()).toThrow(ReferenceError);
    expect(((a = (0, a++)) => a)(4)).toBe(4);
  });

  test("a parameter captured by a closure", () => {
    expect(() => ((a = a++) => () => a)()).toThrow(ReferenceError);
    expect(((a = a++) => () => a)(8)()).toBe(8);
  });

  test("a method parameter", () => {
    class Counter {
      next(a = a++) {
        return a;
      }
    }
    expect(() => new Counter().next()).toThrow(ReferenceError);
    expect(new Counter().next(3)).toBe(3);
    const object = {
      previous(a = a--) {
        return a;
      },
    };
    expect(() => object.previous()).toThrow(ReferenceError);
    expect(object.previous(3)).toBe(3);
  });

  test("the update of an earlier parameter still produces the old value", () => {
    expect(((a, b = a++) => [a, b])(1)).toEqual([2, 1]);
    expect(((a, b = a--) => [a, b])(1n)).toEqual([0n, 1n]);
    expect(((a, b = a++) => [a, b])("4")).toEqual([5, 4]);
  });
});
