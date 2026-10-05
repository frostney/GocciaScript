/*---
description: A var read before its first assignment is undefined after the parameters are destructured
features: [compat-var]
---*/

// ES2026 §10.2.11 FunctionDeclarationInstantiation step 29.c.i.3: every var
// the body declares starts as undefined, after the parameters are bound.
// Values used while destructuring a parameter must not show through it.

describe("var initial value with destructured parameters", () => {
  test("object pattern", () => {
    const fn = ({ a }) => {
      var x, y;
      return [a, x, y];
    };
    expect(fn({ a: 1 })).toEqual([1, undefined, undefined]);
  });

  test("object pattern with two properties", () => {
    const fn = ({ a, b }) => {
      var x;
      return x;
    };
    expect(fn({ a: 1, b: 2 })).toBeUndefined();
  });

  test("array pattern", () => {
    const fn = ([a, b]) => {
      var x, y, z;
      return [x, y, z];
    };
    expect(fn([1, 2])).toEqual([undefined, undefined, undefined]);
  });

  test("object and array patterns together", () => {
    const fn = ({ a }, [b]) => {
      var x;
      return [a, b, x];
    };
    expect(fn({ a: 1 }, [2])).toEqual([1, 2, undefined]);
  });

  test("nested pattern", () => {
    const fn = ({ t: { u } }, [[v]]) => {
      var x, y;
      return [u, v, x, y];
    };
    expect(fn({ t: { u: 3 } }, [[4]])).toEqual([3, 4, undefined, undefined]);
  });

  test("destructured parameters and surplus arguments", () => {
    const fn = ({ a }, [b]) => {
      var x;
      return [a, b, x];
    };
    expect(fn({ a: 1 }, [2], 3, 4)).toEqual([1, 2, undefined]);
  });
});
