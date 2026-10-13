/*---
description: Function declarations read a deleted var-redeclared global as unresolvable, and a function declaration's own global is not deletable
features: [compat-function, compat-var]
---*/

// ES2026 §9.1.1.4.16 CreateGlobalVarBinding leaves the configurable built-in
// property that `var parseInt` redeclares in place, so `delete` removes it.
// The global environment then has no binding (§9.1.1.4.1 HasBinding), GetValue
// throws ReferenceError (§6.2.5.5 GetValue step 2) and `typeof` gives
// "undefined" (§13.5.3.1 step 2.a).
// A function declaration is different: §16.1.7 GlobalDeclarationInstantiation
// calls CreateGlobalFunctionBinding (§9.1.1.4.17) with deletable false, which
// redefines a configurable existing property as non-configurable, so the
// binding cannot be deleted.
// Snapshot: tc39-mcp 0.6.3, es2026 0248456c758431e4bb8e5d26333ff1865123c9cd.

var parseInt = globalThis.parseInt;

function isFinite(value) {
  return "declared:" + value;
}

function readParseInt() {
  return parseInt;
}

function typeofParseInt() {
  return typeof parseInt;
}

function makeReader() {
  return function () {
    return parseInt;
  };
}

function callIsFinite() {
  return isFinite(1);
}

function errorName(fn) {
  try {
    fn();
    return "no error";
  } catch (error) {
    return error.name;
  }
}

const readFromClosure = makeReader();
const warmRead = typeof readParseInt() + ":" + typeof readFromClosure();

const isFiniteDescriptor = Object.getOwnPropertyDescriptor(globalThis, "isFinite");
const deletedParseInt = delete globalThis.parseInt;
const deleteIsFiniteError = errorName(() => delete globalThis.isFinite);

const topLevelReadError = errorName(() => parseInt);

describe("function declarations and deleted globals", () => {
  test("a function declaration reading the deleted var throws ReferenceError", () => {
    expect(warmRead).toBe("function:function");
    expect(deletedParseInt).toBe(true);
    expect(topLevelReadError).toBe("ReferenceError");
    expect(() => readParseInt()).toThrow(ReferenceError);
  });

  test("a closure returned by a function declaration throws ReferenceError", () => {
    expect(() => readFromClosure()).toThrow(ReferenceError);
    expect(() => makeReader()()).toThrow(ReferenceError);
  });

  test("typeof in a function declaration gives undefined", () => {
    expect(typeofParseInt()).toBe("undefined");
  });

  test("a function declaration redefines the configurable global as non-configurable", () => {
    expect(isFiniteDescriptor.configurable).toBe(false);
    expect(isFiniteDescriptor.writable).toBe(true);
    expect(isFiniteDescriptor.enumerable).toBe(true);
    // Strict delete of a non-configurable property throws TypeError
    // (§13.5.1.2 step 4.f).
    expect(deleteIsFiniteError).toBe("TypeError");
    expect(callIsFinite()).toBe("declared:1");
    expect(isFinite(2)).toBe("declared:2");
  });
});
