/*---
description: An object literal has no property limit of its own; past a function's constant limit it is a catchable SyntaxError
features: [ShadowRealm, Function]
---*/

// Each distinct property name is a constant of the function the literal is
// in, and a function holds at most 65,535 constants. An object literal with
// more than 65,535 properties used to stop the process with a range check
// error or an uncatchable fatal error.
const CONSTANT_LIMIT = "Too many constants in one function: more than 65535";
const realm = new ShadowRealm();
const list = (count, item) => Array.from({ length: count }, (_, i) => item(i)).join(", ");

const compileError = (source) => {
  try {
    realm.evaluate(source);
  } catch (error) {
    return error;
  }
  return undefined;
};

describe("object literals and the constant limit", () => {
  test("65,000 distinct property names evaluate", () => {
    const source = `const o = { ${list(65000, (i) => `p${i}: 0`)} }; Object.keys(o).length`;
    expect(realm.evaluate(source)).toBe(65000);
  });

  test("70,000 properties that repeat a few names evaluate", () => {
    const source = `const o = { ${list(70000, (i) => `p${i % 100}: ${i}`)} }; Object.keys(o).length + ":" + o.p99`;
    expect(realm.evaluate(source)).toBe("100:69999");
  });

  test("70,000 computed properties evaluate", () => {
    const source = `const k = "a"; const o = { ${list(70000, (i) => `[k + ${i % 7}]: ${i}`)} }; Object.keys(o).length + ":" + o.a6`;
    expect(realm.evaluate(source)).toBe("7:69999");
  });

  test("70,000 distinct property names are a SyntaxError", () => {
    const error = compileError(`({ ${list(70000, (i) => `p${i}: ${i}`)} })`);
    expect(error instanceof SyntaxError).toBe(true);
    expect(error.message.includes(CONSTANT_LIMIT)).toBe(true);
  });

  test("the SyntaxError can be caught around new Function", () => {
    let caught;
    try {
      new Function(`return { ${list(70000, (i) => `p${i}: 0`)} };`);
    } catch (error) {
      caught = error;
    }
    expect(caught instanceof SyntaxError).toBe(true);
    expect(caught.message.includes(CONSTANT_LIMIT)).toBe(true);
  });
});
