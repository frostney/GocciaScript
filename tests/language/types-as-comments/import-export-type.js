/*---
description: import type and export type declarations are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("import type is skipped", () => {
  import type { Foo } from './nonexistent.js';
  const x = 42;
  expect(x).toBe(42);
});

test("export type is skipped", () => {
  export type { Bar };
  const y = "hello";
  expect(y).toBe("hello");
});
