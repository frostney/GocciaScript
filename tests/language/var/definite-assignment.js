/*---
description: var declarations accept definite assignment assertions, which are erased at runtime
features: [compat-var, types-as-comments]
---*/

test("var with a definite assignment assertion", () => {
  var value!: number;
  value = 5;

  expect(value).toBe(5);
});

test("var assertion with a structured annotation", () => {
  var config!: { retries: number };
  config = { retries: 2 };

  expect(config.retries).toBe(2);
});

test("var binding is undefined before it is assigned", () => {
  var pending!: string;

  expect(pending).toBeUndefined();
});
