/*---
description: Default export of a function expression with compat-function enabled
features: [compat-function, modules, default-exports]
---*/

import defaultDouble from "./helpers/default-function.js";
import namedDefaultFunction, {
  localNamedDefaultFunctionResult,
  localNamedDefaultFunctionType
} from "./helpers/default-named-function.js";

test("default exported function expression can be called", () => {
  expect(defaultDouble(4)).toBe(8);
  expect(defaultDouble.name).toBe("default");
});

test("named default exported function is locally bound", () => {
  expect(namedDefaultFunction(5)).toBe(15);
  expect(namedDefaultFunction.name).toBe("namedDefaultFunction");
  expect(localNamedDefaultFunctionType).toBe("function");
  expect(localNamedDefaultFunctionResult).toBe(9);
});
