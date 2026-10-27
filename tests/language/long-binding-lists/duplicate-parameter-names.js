/*---
description: Duplicate parameter names in long lists are an early error unless the list is simple and non-strict
features: [compat-function, compat-non-strict-mode]
---*/

const COUNT = 3000;
const names = Array.from({ length: COUNT }, (_, i) => `a${i}`);
const list = names.join(", ");
const last = `a${COUNT - 1}`;

const syntaxError = (body) => {
  try {
    new Function(body);
  } catch (error) {
    return error instanceof SyntaxError && error.message.includes("Duplicate parameter name");
  }
  return false;
};

describe("duplicate parameter names in a long list", () => {
  test("a non-strict function with a simple list allows them, and the last one wins", () => {
    const fn = new Function(`return function (${list}, a0) { return a0; };`)();
    const args = Array.from({ length: COUNT + 1 }, (_, i) => i);
    expect(fn(...args)).toBe(COUNT);
    expect(fn.length).toBe(COUNT + 1);
  });

  test("arrows, methods and class methods reject them", () => {
    expect(syntaxError(`return (${list}, a0) => 0;`)).toBe(true);
    expect(syntaxError(`return { m(${list}, ${last}) {} };`)).toBe(true);
    expect(syntaxError(`return class { m(a0, ${list}) {} };`)).toBe(true);
    expect(syntaxError(`return async (${list}, a1) => 0;`)).toBe(true);
  });

  test("a strict function rejects them", () => {
    expect(syntaxError(`return function (${list}, a0) { "use strict"; };`)).toBe(true);
  });

  test("a non-simple list rejects them", () => {
    expect(syntaxError(`return function (${list}, a0 = 1) {};`)).toBe(true);
    expect(syntaxError(`return function (${list}, ...a0) {};`)).toBe(true);
    expect(syntaxError(`return function (${list}, [${last}]) {};`)).toBe(true);
    expect(syntaxError(`return function ({ ${list} }, a5) {};`)).toBe(true);
    expect(syntaxError(`return function ({ x: a0, y: [${last}, a0] }) {};`)).toBe(true);
  });

  test("names that differ only in case or form are distinct", () => {
    const fn = new Function(`return (${list}, A0, é, é) => [a0, A0, é, é];`)();
    const args = Array.from({ length: COUNT }, (_, i) => i);
    expect(fn(...args, "A", "composed", "decomposed")).toEqual([0, "A", "composed", "decomposed"]);
  });

  test("a list without duplicates is accepted in every form", () => {
    expect(syntaxError(`return (${list}) => 0;`)).toBe(false);
    expect(syntaxError(`return class { m(${list}, ...rest) {} };`)).toBe(false);
    expect(syntaxError(`return function ({ ${list} }, [b0], b1 = 1) { "use strict"; };`)).toBe(false);
  });
});
