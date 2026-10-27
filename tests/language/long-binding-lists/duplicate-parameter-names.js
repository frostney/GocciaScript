/*---
description: A long non-strict simple parameter list may repeat a name, and names that differ in case or form are distinct
features: [compat-function, compat-non-strict-mode]
---*/

const COUNT = 3000;
const names = Array.from({ length: COUNT }, (_, i) => `a${i}`);
const list = names.join(", ");

describe("repeated and similar parameter names in a long list", () => {
  test("a non-strict function with a simple list allows them, and the last one wins", () => {
    const fn = new Function(`return function (${list}, a0) { return a0; };`)();
    const args = Array.from({ length: COUNT + 1 }, (_, i) => i);
    expect(fn(...args)).toBe(COUNT);
    expect(fn.length).toBe(COUNT + 1);
  });

  test("names that differ only in case or form are distinct", () => {
    const fn = new Function(`return (${list}, A0, \u00e9, e\u0301) => [a0, A0, \u00e9, e\u0301];`)();
    const args = Array.from({ length: COUNT }, (_, i) => i);
    expect(fn(...args, "A", "composed", "decomposed")).toEqual([0, "A", "composed", "decomposed"]);
  });
});
