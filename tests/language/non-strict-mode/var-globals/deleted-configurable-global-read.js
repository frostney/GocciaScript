/*---
description: Sloppy code reads a deleted var-redeclared global as unresolvable, and a sloppy assignment creates the property again
features: [compat-non-strict-mode, compat-var]
---*/

// ES2026 §9.1.1.4.16 CreateGlobalVarBinding leaves the configurable built-in
// property that `var escape` redeclares in place, so `delete` removes it.
// The global environment then has no binding (§9.1.1.4.1 HasBinding), GetValue
// throws ReferenceError (§6.2.5.5 GetValue step 2) and `typeof` gives
// "undefined" (§13.5.3.1 step 2.a). A sloppy PutValue of the unresolvable
// reference sets the property on the global object (§6.2.5.6 PutValue
// step 2.c), after which reads resolve again.
// Snapshot: tc39-mcp 0.6.3, es2026 0248456c758431e4bb8e5d26333ff1865123c9cd.

var encodeURIComponent = globalThis.encodeURIComponent;
var decodeURIComponent = globalThis.decodeURIComponent;

const readEncode = () => encodeURIComponent;
const makeReader = () => () => decodeURIComponent;
const readDecode = makeReader();
const writeDecode = (value) => {
  decodeURIComponent = value;
};

const errorName = (fn) => {
  try {
    fn();
    return "no error";
  } catch (error) {
    return error.name;
  }
};

const warmRead = typeof readEncode() + ":" + typeof readDecode();
const deletedEncode = delete globalThis.encodeURIComponent;
const deletedDecode = delete globalThis.decodeURIComponent;

const topLevelReadError = errorName(() => encodeURIComponent);
const topLevelTypeof = typeof encodeURIComponent;
const updateError = errorName(() => { encodeURIComponent++; });

encodeURIComponent = "recreated at top level";

describe("sloppy reads and writes of a deleted configurable global", () => {
  test("reads throw ReferenceError once the property is deleted", () => {
    expect(warmRead).toBe("function:function");
    expect(deletedEncode).toBe(true);
    expect(deletedDecode).toBe(true);
    expect(topLevelReadError).toBe("ReferenceError");
    expect(() => readDecode()).toThrow(ReferenceError);
    expect(errorName(() => { decodeURIComponent += "x"; })).toBe("ReferenceError");
    expect(Object.hasOwn(globalThis, "decodeURIComponent")).toBe(false);
  });

  test("typeof gives undefined", () => {
    expect(topLevelTypeof).toBe("undefined");
    expect((() => typeof decodeURIComponent)()).toBe("undefined");
  });

  test("an update reads first and throws without creating the property", () => {
    expect(updateError).toBe("ReferenceError");
  });

  test("a top-level sloppy assignment creates the global property again", () => {
    const descriptor =
      Object.getOwnPropertyDescriptor(globalThis, "encodeURIComponent");
    expect(descriptor.value).toBe("recreated at top level");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(true);
    expect(descriptor.configurable).toBe(true);
    expect(readEncode()).toBe("recreated at top level");
    expect(encodeURIComponent).toBe("recreated at top level");
  });

  test("a sloppy assignment from a function creates the property again", () => {
    writeDecode("recreated in a function");
    expect(Object.hasOwn(globalThis, "decodeURIComponent")).toBe(true);
    expect(readDecode()).toBe("recreated in a function");
    expect(decodeURIComponent).toBe("recreated in a function");
  });
});
