/*---
description: A call of a const-bound local function is typed only when every return of the function has one type
features: [arrow-function, function-keyword, closures, recursion]
---*/

// A const binding always holds the function it was initialized with, so a plain
// call of its name returns what the function's returns produce. Anything the
// compiler cannot show to be one type (a second type, undefined, an async or
// generator function, a callee another binding can shadow) leaves the call
// untyped.

describe("return types of local functions", () => {
  test("a Number-returning arrow", () => {
    const f = (x) => x * 2;
    const r = f("3");
    expect([r === r, r + r]).toEqual([true, 12]);
    expect(f("3") + 1).toBe(7);
  });

  test("returns of two types", () => {
    const g = (x) => (x ? 1 : "s");
    expect(g(0) + g(0)).toBe("ss");
    const h = (x) => {
      if (x) return 1;
      return "s";
    };
    expect(h(0) + h(0)).toBe("ss");
  });

  test("async and generator functions", () => {
    const a = async () => 1;
    expect(typeof (a() + 1)).toBe("string");
    const g = function* () {
      return 1;
    };
    expect(String(g() + "")).toBe("[object Generator]");
    const ag = async function () {
      return 1;
    };
    expect(typeof (ag() + 1)).toBe("string");
  });

  test("a let-bound function can be replaced", () => {
    let f = () => 1;
    f = () => "s";
    expect(f() + f()).toBe("ss");
  });

  test("a shadowing binding of the callee's name", () => {
    const f = () => 1;
    let r;
    {
      const f = () => "s";
      r = f() + f();
    }
    expect(r).toBe("ss");
    const p = (f) => f() + f();
    expect(p(() => "q")).toBe("qq");
  });

  test("a captured callee, recursion and this", () => {
    const sq = (x) => x * x;
    const outer = () => sq("2") + 1;
    expect(outer()).toBe(5);
    const fact = (n) => (n <= 1 ? 1 : n * fact(n - 1));
    expect(fact(5) + 1).toBe(121);
    const self = function () {
      return this;
    };
    expect(self.call("x") + "y").toBe("xy");
  });

  test("an optional call and a member call stay generic", () => {
    const f = () => 1;
    expect(f?.() + 1).toBe(2);
    const o = { f: () => "m" };
    expect(o.f() + o.f()).toBe("mm");
  });

  test("a function that returns a function's result", () => {
    const inner = (x) => x - 0;
    const outer = (x) => inner(x) * 1;
    const r = outer("5");
    expect([r === r, r + r]).toEqual([true, 10]);
  });

  test("a callee declared after the function that calls it", () => {
    const early = () => late() + 1;
    const late = () => "s";
    expect(early()).toBe("s1");
    const caller = () => later() + 1;
    expect(() => caller()).toThrow(ReferenceError);
    const later = () => 2;
    expect(caller()).toBe(3);
  });
});
