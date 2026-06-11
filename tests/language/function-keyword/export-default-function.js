/*---
description: Default export of a function expression with compat-function enabled
features: [compat-function, modules, default-exports]
---*/

import defaultDouble from "./helpers/default-function.js";
import namedDefaultFunction, {
  localNamedDefaultFunctionResult,
  localNamedDefaultFunctionType
} from "./helpers/default-named-function.js";
import defaultFunctionBeforeStatement, {
  defaultFunctionFollowingStatementHit
} from "./helpers/default-function-following-statement.js";
import namedDefaultFunctionBeforeStatement, {
  defaultNamedFunctionFollowingStatementHit,
  localFollowingNamedDefaultFunctionResult
} from "./helpers/default-named-function-following-statement.js";
import hoistedNamedDefault, {
  afterCall as hoistedNamedDefaultAfterCall,
  beforeCall as hoistedNamedDefaultBeforeCall,
  beforeType as hoistedNamedDefaultBeforeType
} from "./helpers/default-named-function-hoisted.js";
import defaultGeneratorBeforeStatement, {
  defaultGeneratorFollowingStatementHit
} from "./helpers/default-generator-following-statement.js";
import namedDefaultGeneratorBeforeStatement, {
  defaultNamedGeneratorFollowingStatementHit,
  localFollowingNamedDefaultGeneratorType
} from "./helpers/default-named-generator-following-statement.js";
import CapturedDefaultClass, {
  readCapturedDefaultClassName
} from "./helpers/default-named-class-hoisted-capture.js";

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

test("default exported function declaration can be followed by another statement", () => {
  expect(defaultFunctionBeforeStatement(4)).toBe(5);
  expect(defaultFunctionBeforeStatement.name).toBe("default");
  expect(defaultFunctionFollowingStatementHit).toBe(1);
});

test("named default exported function declaration can be followed by another statement", () => {
  expect(namedDefaultFunctionBeforeStatement(4)).toBe(8);
  expect(namedDefaultFunctionBeforeStatement.name).toBe("followingNamedDefaultFunction");
  expect(localFollowingNamedDefaultFunctionResult).toBe(10);
  expect(defaultNamedFunctionFollowingStatementHit).toBe(1);
});

test("named default exported function declaration is hoisted", () => {
  expect(hoistedNamedDefault.name).toBe("hoistedNamedDefault");
  expect(hoistedNamedDefaultBeforeType).toBe("function");
  expect(hoistedNamedDefaultBeforeCall).toBe(7);
  expect(hoistedNamedDefaultAfterCall).toBe(8);
});

test("default exported generator declaration can be followed by another statement", () => {
  expect(defaultGeneratorBeforeStatement().next()).toEqual({ value: "default generator", done: false });
  expect(defaultGeneratorFollowingStatementHit).toBe(1);
});

test("named default exported generator declaration can be followed by another statement", () => {
  expect(namedDefaultGeneratorBeforeStatement().next()).toEqual({ value: "named generator", done: false });
  expect(localFollowingNamedDefaultGeneratorType).toBe("function");
  expect(defaultNamedGeneratorFollowingStatementHit).toBe(1);
});

test("hoisted functions capture named default class declarations", () => {
  expect(CapturedDefaultClass.name).toBe("CapturedDefaultClass");
  expect(readCapturedDefaultClassName()).toBe("CapturedDefaultClass");
});
