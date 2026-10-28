/*---
description: Destructuring patterns with thousands of bindings bind every name, in functions and at the top level of a script
features: [compat-function, compat-non-strict-mode, compat-var, compat-traditional-for-loop, ShadowRealm]
---*/

const COUNT = 3000;
const names = Array.from({ length: COUNT }, (_, i) => `b${i}`);
const list = names.join(", ");
const last = `b${COUNT - 1}`;
const source = Object.fromEntries(names.map((name, i) => [name, i]));
const values = Array.from({ length: COUNT }, (_, i) => i);

describe("a pattern with thousands of bindings", () => {
  test("object and array patterns bind every name in a function", () => {
    for (const kind of ["const", "let", "var"]) {
      const object = new Function("o", `${kind} { ${list} } = o; return [b0, ${last}];`);
      const array = new Function("a", `${kind} [${list}] = a; return [b0, ${last}];`);
      expect(object(source)).toEqual([0, COUNT - 1]);
      expect(array(values)).toEqual([0, COUNT - 1]);
    }
  });

  test("nested patterns with defaults bind every name", () => {
    const nested = names
      .map((name, i) => (i % 2 ? `k${i}: [${name} = ${i}]` : `k${i}: { v: ${name} = -${i} }`))
      .join(", ");
    const fn = new Function("o", `const { ${nested} } = o; return [b0, b1, ${last}];`);
    const empty = new Proxy({}, { get: () => [] });
    // Every default applies: the odd keys get an empty array, the even ones
    // an array whose v property is undefined.
    expect(fn(empty)).toEqual([-0, 1, COUNT - 1]);
  });

  test("a rest element collects what the names leave", () => {
    const fn = new Function("o", `const { ${list}, ...rest } = o; return [${last}, rest];`);
    expect(fn({ ...source, extra: "x" })).toEqual([COUNT - 1, { extra: "x" }]);
    const array = new Function("a", `const [${list}, ...rest] = a; return [${last}, rest];`);
    expect(array([...values, "x", "y"])).toEqual([COUNT - 1, ["x", "y"]]);
  });

  test("for-of and catch patterns bind every name", () => {
    const forOf = new Function("items", `for (const { ${list} } of items) { return [b0, ${last}]; }`);
    expect(forOf([source])).toEqual([0, COUNT - 1]);
    const caught = new Function("o", `try { throw o; } catch ({ ${list} }) { return [b0, ${last}]; }`);
    expect(caught(source)).toEqual([0, COUNT - 1]);
  });

  test("a static block and a for statement's let bind every name", () => {
    const C = new Function("o", `return class { static { const { ${list} } = o; this.ends = [b0, ${last}]; } };`)(source);
    expect(C.ends).toEqual([0, COUNT - 1]);
    const loop = new Function("o", `for (let { ${list} } = o; ; ) { return [b0, ${last}]; }`);
    expect(loop(source)).toEqual([0, COUNT - 1]);
  });

  test("a top-level pattern of a script binds every name", () => {
    const realm = new ShadowRealm();
    const sum = realm.evaluate(
      `const { ${list} } = { b0: 1, ${last}: 2 }; let [c0, ...cs] = [3, 4]; var { v0, ...vs } = {}; b0 + ${last} + c0 + cs[0];`,
    );
    expect(sum).toBe(10);
  });

  test("a var pattern may bind a name twice, and the later binding wins", () => {
    const fn = new Function(`var { ${list}, x: b0 } = { b0: 1, x: 2 }; return b0;`);
    expect(fn()).toBe(2);
  });
});
