/*---
description: Annex B block function var activation excludes async and generator declarations
features: [compat-function, compat-non-strict-mode, compat-var]
---*/

test("ordinary block function declarations update the nearest var binding", () => {
  const result = new Function(`
    { function f() { return "ordinary"; } }
    return typeof f + ":" + f();
  `)();

  expect(result).toBe("function:ordinary");
});

test("generator block declarations stay block-scoped", () => {
  const blockResult = new Function(`
    { function* g() { yield 1; } }
    return typeof g;
  `)();
  const switchResult = new Function(`
    switch (1) {
      case 1:
        function* g() { yield 1; }
    }
    return typeof g;
  `)();

  expect(blockResult).toBe("undefined");
  expect(switchResult).toBe("undefined");
});

test("async block declarations stay block-scoped", () => {
  const asyncResult = new Function(`
    { async function af() { return 1; } }
    return typeof af;
  `)();
  const asyncGeneratorResult = new Function(`
    { async function* ag() { yield 1; } }
    return typeof ag;
  `)();

  expect(asyncResult).toBe("undefined");
  expect(asyncGeneratorResult).toBe("undefined");
});
