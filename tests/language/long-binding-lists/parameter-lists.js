/*---
description: Functions with thousands of parameters bind, default, count and collect them like short ones
features: [compat-function, compat-non-strict-mode]
---*/

const COUNT = 3000;
const names = Array.from({ length: COUNT }, (_, i) => `a${i}`);
const list = names.join(", ");
const args = Array.from({ length: COUNT }, (_, i) => i * 2);
const last = `a${COUNT - 1}`;

describe("a parameter list with thousands of names", () => {
  test("every form binds the first and last parameter", () => {
    const forms = [
      `return (${list}) => [a0, ${last}];`,
      `return function (${list}) { return [a0, ${last}]; };`,
      `function f(${list}) { return [a0, ${last}]; } return f;`,
      `return { m(${list}) { return [a0, ${last}]; } }.m;`,
      `return new (class { m(${list}) { return [a0, ${last}]; } })().m;`,
    ];
    for (const body of forms) {
      const fn = new Function(body)();
      expect(fn(...args)).toEqual([0, (COUNT - 1) * 2]);
      expect(fn.length).toBe(COUNT);
    }
  });

  test("a default can read every parameter before it", () => {
    const defaults = names.map((name, i) => (i === 0 ? name : `${name} = a${i - 1} + 1`));
    const fn = new Function(`return (${defaults.join(", ")}) => ${last};`)();
    expect(fn(5)).toBe(5 + COUNT - 1);
    expect(fn(5, 100)).toBe(100 + COUNT - 2);
    // length counts the parameters before the first default.
    expect(fn.length).toBe(1);
  });

  test("a default cannot read a parameter after it", () => {
    const fn = new Function(`return (a0 = ${last}, ${names.slice(1).join(", ")}) => a0;`)();
    expect(() => fn()).toThrow(ReferenceError);
    expect(fn(1)).toBe(1);
  });

  test("a rest parameter collects what the named ones leave", () => {
    const fn = new Function(`return (${list}, ...rest) => [${last}, rest];`)();
    expect(fn(...args, "x", "y")).toEqual([(COUNT - 1) * 2, ["x", "y"]]);
    expect(fn(...args)).toEqual([(COUNT - 1) * 2, []]);
    expect(fn.length).toBe(COUNT);
  });

  test("pattern parameters bind their names", () => {
    const patterns = names.map((name, i) => (i % 2 ? `[${name}]` : `{ v: ${name} }`));
    const fn = new Function(`return (${patterns.join(", ")}) => [a0, a1, ${last}];`)();
    const values = Array.from({ length: COUNT }, (_, i) => (i % 2 ? [i] : { v: i }));
    expect(fn(...values)).toEqual([0, 1, COUNT - 1]);
    expect(fn.length).toBe(COUNT);
  });

  test("an async arrow binds a large object pattern", async () => {
    const fn = new Function(`return async ({ ${list} }) => a0 + ${last};`)();
    expect(await fn({ a0: 1, [last]: 2 })).toBe(3);
  });

  test("a block function named like a late parameter leaves it alone", () => {
    const fn = new Function(`return function (${list}) { { function ${last}() {} } return ${last}; };`)();
    expect(fn(...args)).toBe((COUNT - 1) * 2);
  });

  test("a block function with another name still gets its var binding", () => {
    const fn = new Function(`return function (${list}) { { function other() { return a0; } } return other(); };`)();
    expect(fn(...args)).toBe(0);
  });
});
