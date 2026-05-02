/*---
description: var allows redeclaration without errors
features: [compat-var]
---*/

var __gocciaGlobalVarUndefinedInitializer = 19;
var __gocciaGlobalVarUndefinedInitializer = undefined;

const __gocciaOriginalGlobalPrototypeForVar = Object.getPrototypeOf(globalThis);
let __gocciaInheritedGlobalVarDeclarationOwn;
let __gocciaInheritedGlobalVarDeclarationValue;
try {
  const __gocciaInheritedGlobalVarPrototype = Object.create(__gocciaOriginalGlobalPrototypeForVar);
  __gocciaInheritedGlobalVarPrototype.__gocciaInheritedGlobalVarDeclaration = 37;
  Object.setPrototypeOf(globalThis, __gocciaInheritedGlobalVarPrototype);
  var __gocciaInheritedGlobalVarDeclaration;
  __gocciaInheritedGlobalVarDeclarationOwn = Object.hasOwn(globalThis, "__gocciaInheritedGlobalVarDeclaration");
  __gocciaInheritedGlobalVarDeclarationValue = globalThis.__gocciaInheritedGlobalVarDeclaration;
  delete globalThis.__gocciaInheritedGlobalVarDeclaration;
} finally {
  Object.setPrototypeOf(globalThis, __gocciaOriginalGlobalPrototypeForVar);
}

var __gocciaDeletedInheritedGlobalVarDeclaration = 42;
delete globalThis.__gocciaDeletedInheritedGlobalVarDeclaration;
const __gocciaDeletedOriginalGlobalPrototypeForVar = Object.getPrototypeOf(globalThis);
let __gocciaDeletedInheritedGlobalVarDeclarationOwn;
let __gocciaDeletedInheritedGlobalVarDeclarationValue;
try {
  const __gocciaDeletedInheritedGlobalVarPrototype = Object.create(__gocciaDeletedOriginalGlobalPrototypeForVar);
  __gocciaDeletedInheritedGlobalVarPrototype.__gocciaDeletedInheritedGlobalVarDeclaration = 99;
  Object.setPrototypeOf(globalThis, __gocciaDeletedInheritedGlobalVarPrototype);
  var __gocciaDeletedInheritedGlobalVarDeclaration;
  __gocciaDeletedInheritedGlobalVarDeclarationOwn = Object.hasOwn(globalThis, "__gocciaDeletedInheritedGlobalVarDeclaration");
  __gocciaDeletedInheritedGlobalVarDeclarationValue = globalThis.__gocciaDeletedInheritedGlobalVarDeclaration;
  delete globalThis.__gocciaDeletedInheritedGlobalVarDeclaration;
} finally {
  Object.setPrototypeOf(globalThis, __gocciaDeletedOriginalGlobalPrototypeForVar);
}

let __gocciaHoistedGlobalVarReadSucceeded = false;
let __gocciaHoistedGlobalVarInitialValue = "not-read";
try {
  __gocciaHoistedGlobalVarInitialValue = __gocciaHoistedGlobalVar;
  __gocciaHoistedGlobalVarReadSucceeded = true;
} catch (e) {
  __gocciaHoistedGlobalVarInitialValue = e.name;
}
var __gocciaHoistedGlobalVar = 77;

__gocciaHoistedAssignedGlobalVar = 12;
var __gocciaHoistedAssignedGlobalVar;

test("var redeclaration does not throw", () => {
  var x = 1;
  var x = 2;
  expect(x).toBe(2);
});

test("var redeclaration with different values", () => {
  var a = "first";
  var a = "second";
  var a = "third";
  expect(a).toBe("third");
});

test("var redeclaration without initializer preserves value", () => {
  var x = 42;
  var x;
  expect(x).toBe(42);
});

test("top-level var undefined initializer updates global-backed binding", () => {
  expect(__gocciaGlobalVarUndefinedInitializer).toBeUndefined();
  expect(globalThis.__gocciaGlobalVarUndefinedInitializer).toBeUndefined();
});

test("declaration-only top-level var creates own property over inherited global", () => {
  expect(__gocciaInheritedGlobalVarDeclarationOwn).toBe(true);
  expect(__gocciaInheritedGlobalVarDeclarationValue).toBeUndefined();
});

test("declaration-only redeclared top-level var does not reuse stale register value", () => {
  expect(__gocciaDeletedInheritedGlobalVarDeclarationOwn).toBe(true);
  expect(__gocciaDeletedInheritedGlobalVarDeclarationValue).toBeUndefined();
});

test("top-level var is visible before its declaration executes", () => {
  expect(__gocciaHoistedGlobalVarReadSucceeded).toBe(true);
  expect(__gocciaHoistedGlobalVarInitialValue).toBeUndefined();
  expect(__gocciaHoistedGlobalVar).toBe(77);
  expect(globalThis.__gocciaHoistedGlobalVar).toBe(77);
});

test("declaration-only top-level var preserves assignment before declaration", () => {
  expect(__gocciaHoistedAssignedGlobalVar).toBe(12);
  expect(globalThis.__gocciaHoistedAssignedGlobalVar).toBe(12);
});
