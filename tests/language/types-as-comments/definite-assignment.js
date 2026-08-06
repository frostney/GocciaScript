/*---
description: Definite assignment assertions on variable declarations are parsed and ignored at runtime
features: [types-as-comments]
---*/

// The three rejection rules ('!' without an annotation, '!' with an initializer,
// '!' on a const) are parse errors and are covered in scripts/test-cli-parser.ts.
// The same-line restriction on '!' is covered in
// tests/language/asi/definite-assignment-restriction.js.

describe("definite assignment assertions", () => {

test("let with a definite assignment assertion", () => {
  let value!: number;
  value = 5;

  expect(value).toBe(5);
});

test("structured annotation after the assertion", () => {
  let config!: { retries: number };
  config = { retries: 2 };

  expect(config.retries).toBe(2);
});

test("union annotation after the assertion", () => {
  let entry!: string | null;
  entry = "ready";

  expect(entry).toBe("ready");
});

test("mixed declarators in one statement", () => {
  let first!: string, second: number = 2, third!: boolean;
  first = "x";
  third = true;

  expect(first).toBe("x");
  expect(second).toBe(2);
  expect(third).toBe(true);
});

test("the binding is undefined before it is assigned", () => {
  let pending!: number;

  expect(pending).toBeUndefined();
});

test("the annotation is still enforced after the assertion", () => {
  let counter!: number;
  counter = 1;

  expect(counter).toBe(1);
  expect(() => { counter = "two"; }).toThrow(TypeError);
});

test("logical not is unaffected", () => {
  let flag = true;

  expect(!flag).toBe(false);
  expect(!!flag).toBe(true);
});

});
