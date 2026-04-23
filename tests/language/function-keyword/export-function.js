/*---
description: Exported function declarations
features: [compat-function]
---*/

import { greet, add } from "./helpers/exported-functions.js";

test("exported function declaration can be called", () => {
  expect(add(3, 4)).toBe(7);
});

test("exported function declaration is callable by name", () => {
  expect(greet("test")).toBe("Hello, test");
});
