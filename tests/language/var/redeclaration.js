/*---
description: var allows redeclaration without errors
features: [compat-var]
---*/

var __gocciaGlobalVarUndefinedInitializer = 19;
var __gocciaGlobalVarUndefinedInitializer = undefined;

const __gocciaOriginalGlobalPrototypeForVar = Object.getPrototypeOf(globalThis);
const __gocciaInheritedGlobalVarPrototype = Object.create(__gocciaOriginalGlobalPrototypeForVar);
__gocciaInheritedGlobalVarPrototype.__gocciaInheritedGlobalVarDeclaration = 37;
Object.setPrototypeOf(globalThis, __gocciaInheritedGlobalVarPrototype);
var __gocciaInheritedGlobalVarDeclaration;
const __gocciaInheritedGlobalVarDeclarationOwn = Object.hasOwn(globalThis, "__gocciaInheritedGlobalVarDeclaration");
const __gocciaInheritedGlobalVarDeclarationValue = globalThis.__gocciaInheritedGlobalVarDeclaration;
delete globalThis.__gocciaInheritedGlobalVarDeclaration;
Object.setPrototypeOf(globalThis, __gocciaOriginalGlobalPrototypeForVar);

var __gocciaDeletedInheritedGlobalVarDeclaration = 42;
delete globalThis.__gocciaDeletedInheritedGlobalVarDeclaration;
const __gocciaDeletedOriginalGlobalPrototypeForVar = Object.getPrototypeOf(globalThis);
const __gocciaDeletedInheritedGlobalVarPrototype = Object.create(__gocciaDeletedOriginalGlobalPrototypeForVar);
__gocciaDeletedInheritedGlobalVarPrototype.__gocciaDeletedInheritedGlobalVarDeclaration = 99;
Object.setPrototypeOf(globalThis, __gocciaDeletedInheritedGlobalVarPrototype);
var __gocciaDeletedInheritedGlobalVarDeclaration;
const __gocciaDeletedInheritedGlobalVarDeclarationOwn = Object.hasOwn(globalThis, "__gocciaDeletedInheritedGlobalVarDeclaration");
const __gocciaDeletedInheritedGlobalVarDeclarationValue = globalThis.__gocciaDeletedInheritedGlobalVarDeclaration;
delete globalThis.__gocciaDeletedInheritedGlobalVarDeclaration;
Object.setPrototypeOf(globalThis, __gocciaDeletedOriginalGlobalPrototypeForVar);

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
