/*---
description: A read of a script var whose configurable global property was deleted throws ReferenceError
features: [compat-var]
---*/

// ES2026 §9.1.1.4.16 CreateGlobalVarBinding defines no property when the
// global object already has one, so `var parseFloat` leaves the configurable
// built-in property in place and `delete` removes it. The global environment
// then has no binding of the name (§9.1.1.4.1 HasBinding), the reference is
// unresolvable, and GetValue throws ReferenceError (§6.2.5.5 GetValue step 2).
// `typeof` of an unresolvable reference is "undefined" (§13.5.3.1 step 2.a).
// Strict PutValue throws too (§6.2.5.6 PutValue step 2.a).
// Snapshot: tc39-mcp 0.6.3, es2026 0248456c758431e4bb8e5d26333ff1865123c9cd.

var parseFloat = globalThis.parseFloat;
var decodeURI = globalThis.decodeURI;
let encodeURI = "lexical";
const lexicalConst = "const";

const readParseFloat = () => parseFloat;
const typeofParseFloat = () => typeof parseFloat;
const makeReader = () => () => decodeURI;
const readDecodeURI = makeReader();

const errorName = (fn) => {
  try {
    fn();
    return "no error";
  } catch (error) {
    return error.name;
  }
};

// Warm the read caches before the properties go away.
let warmReads = 0;
for (const _ of [1, 2, 3]) {
  if (typeof readParseFloat() === "function") warmReads = warmReads + 1;
  if (typeof readDecodeURI() === "function") warmReads = warmReads + 1;
}

const parseFloatWasConfigurable =
  Object.getOwnPropertyDescriptor(globalThis, "parseFloat").configurable;
const deletedParseFloat = delete globalThis.parseFloat;
const deletedDecodeURI = delete globalThis.decodeURI;
const deletedEncodeURI = delete globalThis.encodeURI;
const deletedLexicalConst = delete globalThis.lexicalConst;

const topLevelReadError = errorName(() => parseFloat);
let topLevelTypeof = "";
topLevelTypeof = typeof parseFloat;

describe("reading a deleted configurable global redeclared by var", () => {
  test("the var keeps the property configurable, so delete removes it", () => {
    expect(warmReads).toBe(6);
    expect(parseFloatWasConfigurable).toBe(true);
    expect(deletedParseFloat).toBe(true);
    expect(deletedDecodeURI).toBe(true);
    expect(Object.hasOwn(globalThis, "parseFloat")).toBe(false);
    expect("decodeURI" in globalThis).toBe(false);
  });

  test("a top-level read throws ReferenceError", () => {
    expect(topLevelReadError).toBe("ReferenceError");
    expect(() => parseFloat("1")).toThrow(ReferenceError);
  });

  test("a read from a function throws ReferenceError", () => {
    expect(() => readParseFloat()).toThrow(ReferenceError);
    expect(() => parseFloat.name).toThrow(ReferenceError);
  });

  test("a read from a closure throws ReferenceError", () => {
    expect(() => readDecodeURI()).toThrow(ReferenceError);
    expect(() => makeReader()()).toThrow(ReferenceError);
  });

  test("typeof gives undefined", () => {
    expect(topLevelTypeof).toBe("undefined");
    expect(typeofParseFloat()).toBe("undefined");
    expect(typeof decodeURI).toBe("undefined");
  });

  test("compound assignment and update read first and throw", () => {
    expect(errorName(() => { parseFloat += 1; })).toBe("ReferenceError");
    expect(errorName(() => { parseFloat++; })).toBe("ReferenceError");
    expect(errorName(() => { decodeURI ??= 1; })).toBe("ReferenceError");
    expect(Object.hasOwn(globalThis, "parseFloat")).toBe(false);
    expect(Object.hasOwn(globalThis, "decodeURI")).toBe(false);
  });

  test("a strict assignment throws ReferenceError and creates nothing", () => {
    expect(errorName(() => { parseFloat = 1; })).toBe("ReferenceError");
    expect(Object.hasOwn(globalThis, "parseFloat")).toBe(false);
  });

  test("a new global property makes the name resolve again", () => {
    globalThis.decodeURI = "restored";
    expect(readDecodeURI()).toBe("restored");
    expect(decodeURI).toBe("restored");
    delete globalThis.decodeURI;
    expect(() => readDecodeURI()).toThrow(ReferenceError);
  });

  test("let and const bindings are not global properties and stay readable", () => {
    // §16.1.7 GlobalDeclarationInstantiation creates them in the declarative
    // record; deleting the shadowed global property leaves them in place.
    expect(deletedEncodeURI).toBe(true);
    expect(deletedLexicalConst).toBe(true);
    expect(encodeURI).toBe("lexical");
    expect(lexicalConst).toBe("const");
    expect((() => encodeURI)()).toBe("lexical");
  });
});
