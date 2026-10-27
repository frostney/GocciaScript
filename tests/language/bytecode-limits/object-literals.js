/*---
description: An object literal has no property limit of its own; past a function's constant limit it is a catchable SyntaxError
features: [ShadowRealm, Function]
---*/

// Each distinct property name and string value is a constant of the function
// the literal is in, and a function holds at most 65,535 constants. An object
// literal with more than 65,535 properties used to stop the process with a
// range check error or an uncatchable fatal error.
const CONSTANT_LIMIT = "Too many constants in one function: more than 65535";
const realm = new ShadowRealm();
const list = (count, item, separator = ", ") =>
  Array.from({ length: count }, (_, i) => item(i)).join(separator);

const compileError = (source) => {
  try {
    realm.evaluate(source);
  } catch (error) {
    return error;
  }
  return undefined;
};

const expectLimit = (source, message) => {
  const error = compileError(source);
  expect(error instanceof SyntaxError).toBe(true);
  expect(error.message.includes(message)).toBe(true);
};

describe("object literals and the constant limit", () => {
  test("20,000 distinct property names evaluate", () => {
    expect(realm.evaluate(`const o = { ${list(20000, (i) => `p${i}: 0`)} }; Object.keys(o).length`)).toBe(20000);
  });

  test("66,000 properties that repeat a few names evaluate", () => {
    expect(realm.evaluate(`const o = { ${list(66000, (i) => `p${i % 100}: ${i % 1000}`)} }; Object.keys(o).length + ":" + o.p99`)).toBe("100:999");
  });

  test("33,000 properties with distinct names and string values are a SyntaxError", () => {
    expectLimit(`({ ${list(33000, (i) => `p${i}: "v${i}"`)} })`, CONSTANT_LIMIT);
  });

  test("the SyntaxError for a literal inside a function can be caught around new Function", () => {
    let caught;
    try {
      new Function(`return () => ({ ${list(33000, (i) => `p${i}: "v${i}"`)} });`);
    } catch (error) {
      caught = error;
    }
    expect(caught instanceof SyntaxError).toBe(true);
    expect(caught.message.includes(CONSTANT_LIMIT)).toBe(true);
  });
});
