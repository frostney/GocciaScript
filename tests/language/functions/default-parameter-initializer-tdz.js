/*---
description: A parameter is in its temporal dead zone while its own default initializer runs, so reading or writing it there throws ReferenceError however the initializer is built, and so does reaching a later parameter
features: [default-parameters, destructuring, logical-assignment, temporal-dead-zone]
---*/

describe("default parameter initializer and the parameter's temporal dead zone", () => {
  test("assigning the parameter in its own initializer throws", () => {
    expect(() => ((c = (c = 7)) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (c = 7)) => c)(undefined)).toThrow(ReferenceError);
    expect(() => ((c = (0, c = 7)) => c)()).toThrow(ReferenceError);
    expect(() => ((c = [(c = 7)]) => c)()).toThrow(ReferenceError);
    expect(() => ((c = { v: (c = 7) }) => c)()).toThrow(ReferenceError);
  });

  test("compound and logical assignment of the parameter throws", () => {
    expect(() => ((c = (c += 1)) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (c ||= 1)) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (c &&= 1)) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (c ??= 1)) => c)()).toThrow(ReferenceError);
  });

  test("updating the parameter throws", () => {
    expect(() => ((c = c++) => c)()).toThrow(ReferenceError);
    expect(() => ((c = --c) => c)()).toThrow(ReferenceError);
    expect(() => ((c = [c++]) => c)()).toThrow(ReferenceError);
  });

  test("a destructuring assignment to the parameter throws", () => {
    expect(() => ((c = ([c] = [7])) => c)()).toThrow(ReferenceError);
    expect(() => ((c = ({ c } = { c: 7 })) => c)()).toThrow(ReferenceError);
    expect(() => ((c = ({ v: c } = { v: 7 })) => c)()).toThrow(ReferenceError);
    expect(() => ((c = ([...c] = [7])) => c)()).toThrow(ReferenceError);
  });

  test("reading the parameter throws whatever the initializer builds first", () => {
    expect(() => ((c = c) => c)()).toThrow(ReferenceError);
    expect(() => ((c = [c]) => c)()).toThrow(ReferenceError);
    expect(() => ((c = [1, c]) => c)()).toThrow(ReferenceError);
    expect(() => ((c = [...[1], c]) => c)()).toThrow(ReferenceError);
    expect(() => ((c = { v: c }) => c)()).toThrow(ReferenceError);
    expect(() => ((c = `${c}`) => c)()).toThrow(ReferenceError);
    expect(() => ((c = 1 && c) => c)()).toThrow(ReferenceError);
    expect(() => ((c = 0 || c) => c)()).toThrow(ReferenceError);
    expect(() => ((c = null ?? c) => c)()).toThrow(ReferenceError);
    expect(() => ((c = 1 + c) => c)()).toThrow(ReferenceError);
    expect(() => ((c = typeof c) => c)()).toThrow(ReferenceError);
    const identity = (value) => value;
    expect(() => ((c = identity(c)) => c)()).toThrow(ReferenceError);
  });

  test("a closure called during the initializer reaches the parameter in its dead zone", () => {
    expect(() => ((c = (() => { c = 7; return 1; })()) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (() => c)()) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (() => c += 1)()) => c)()).toThrow(ReferenceError);
    expect(() => ((c = (() => c++)()) => c)()).toThrow(ReferenceError);
    expect(() => ((c = { v: (() => c)() }) => c)()).toThrow(ReferenceError);
    expect(() => ((c = { v: (() => { c = 5; })() }) => c)()).toThrow(ReferenceError);
    expect(() => ((c = 1 && (() => c)()) => c)()).toThrow(ReferenceError);
    expect(() => ((c = `${(() => c)()}`) => c)()).toThrow(ReferenceError);
  });

  test("a closure from an earlier initializer reaches the parameter in its dead zone", () => {
    expect(() => ((g = () => c, c = { v: g() }) => c)()).toThrow(ReferenceError);
    expect(() => ((h = 1, g = () => c, c = h && g()) => c)()).toThrow(ReferenceError);
    expect(() => ((g = () => { c = 5; }, c = (g(), 2)) => c)()).toThrow(ReferenceError);
    expect(() => (({ g = () => c } = {}, c = [g()]) => c)()).toThrow(ReferenceError);
  });

  test("assigning or reading a later parameter throws", () => {
    expect(() => ((a = (b = 7), b) => [a, b])()).toThrow(ReferenceError);
    expect(() => ((a = b, b) => [a, b])()).toThrow(ReferenceError);
    expect(() => ((a = (b += 1), b) => a)()).toThrow(ReferenceError);
    expect(() => ((a = b++, b) => a)()).toThrow(ReferenceError);
    expect(() => ((a = ([b] = [7]), b) => a)()).toThrow(ReferenceError);
    expect(() => ((a = (() => { b = 7; return 1; })(), b) => a)()).toThrow(ReferenceError);
  });

  test("bindings of a pattern parameter stay in their dead zone during its initializers", () => {
    expect(() => (({ x } = (x = 7, {})) => x)()).toThrow(ReferenceError);
    expect(() => (({ x = (x = 7) }) => x)({})).toThrow(ReferenceError);
    expect(() => (({ x = [x] }) => x)({})).toThrow(ReferenceError);
    expect(() => (({ x = (y = 7), y }) => x)({})).toThrow(ReferenceError);
    expect(() => (([x = (x = 7)]) => x)([])).toThrow(ReferenceError);
    expect(() => (([a] = [a]) => a)()).toThrow(ReferenceError);
    expect(() => (({ a } = { a: a }) => a)()).toThrow(ReferenceError);
  });

  test("methods, accessors and rest parameters", () => {
    const object = {
      method(c = (c = 7)) {
        return c;
      },
      set value(c = [c]) {
        this.stored = c;
      },
    };
    expect(() => object.method()).toThrow(ReferenceError);
    expect(() => {
      object.value = undefined;
    }).toThrow(ReferenceError);
    class Box {
      constructor(c = { v: c }) {
        this.c = c;
      }
      static make(c = (c = 7)) {
        return c;
      }
    }
    expect(() => new Box()).toThrow(ReferenceError);
    expect(() => Box.make()).toThrow(ReferenceError);
    expect(() => ((c = (c = 7), ...rest) => [c, rest])()).toThrow(ReferenceError);
  });

  test("a passed argument skips the initializer", () => {
    expect(((c = (c = 7)) => c)(3)).toBe(3);
    expect(((c = [c]) => c)(3)).toBe(3);
    expect(((c = ([c] = [7])) => c)(3)).toBe(3);
    expect(((c = 1 && c) => c)(null)).toBe(null);
  });

  test("an initializer may read and write earlier parameters", () => {
    expect(((a, b = (a = 5)) => [a, b])(1)).toEqual([5, 5]);
    expect(((a, b = a++) => [a, b])(1)).toEqual([2, 1]);
    expect(((a, b = [a, (a = 3)]) => [a, b])(1)).toEqual([3, [1, 3]]);
    expect(((a, b = ([a] = [9])) => a)(1)).toBe(9);
    expect(((a = 2, b = { v: a }) => b.v)()).toBe(2);
  });

  test("an initializer that does not reach its own parameter is unaffected", () => {
    expect(((c = [1, 2]) => c)()).toEqual([1, 2]);
    expect(((c = { v: 1 }) => c.v)()).toBe(1);
    expect(((c = 0 || "x") => c)()).toBe("x");
    expect(((c = () => "made") => c())()).toBe("made");
    expect(((c = () => c) => c() === c)()).toBe(true);
    expect(((c = [() => c]) => c[0]() === c)()).toBe(true);
    expect(((c = 1, g = () => { c = 9; }) => { g(); return c; })()).toBe(9);
    expect(((g = () => c, c = 4) => g())()).toBe(4);
  });
});
