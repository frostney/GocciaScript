/*---
description: A const takes an inferred Number type only from a proven Number initializer
features: [const, closures, destructuring, temporal-dead-zone]
---*/

describe("const bindings", () => {
  test("a captured const keeps its type in the closure", () => {
    const k = { valueOf: () => 2 };
    const c = 3 * k;
    const f = () => c + c;
    expect(f()).toBe(12);
    const g = () => () => c === c;
    expect(g()()).toBe(true);
  });

  test("a closure created before the const is initialized", () => {
    const f = () => c + c;
    expect(() => f()).toThrow(ReferenceError);
    const c = 2 * 2;
    expect(f()).toBe(8);
  });

  test("a const initialized from a value that may not be a Number", () => {
    const f = (v) => {
      const a = v - 0;
      const b = v;
      const c = v + 1;
      return [a === a, b + b, c + c];
    };
    expect(f("2")).toEqual([true, "22", "2121"]);
    expect(f(2)).toEqual([true, 4, 6]);
  });

  test("a const chain through other Number consts", () => {
    const k = { valueOf: () => 2 };
    const a = k * 1;
    const b = a * 3;
    const c = -b;
    expect([c === c, c + c]).toEqual([true, -12]);
  });

  test("a destructured const is not assumed to be a Number", () => {
    const { a, b } = { a: "x", b: 1 };
    const [c] = ["y"];
    expect(a + a).toBe("xx");
    expect(b + b).toBe(2);
    expect(c + c).toBe("yy");
    const inner = (o) => {
      const { n } = o;
      return n + n;
    };
    expect(inner({ n: "z" })).toBe("zz");
  });

  test("a const in a loop body is new each iteration", () => {
    const seen = [];
    for (const v of [1, "a"]) {
      const w = v;
      seen.push(w + w);
    }
    expect(seen).toEqual([2, "aa"]);
  });
});
