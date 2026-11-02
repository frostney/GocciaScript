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
// The same list from a replacement pattern in which $& is the item's index,
// built without a guest callback per item.
const numbered = (count, pattern) => [...Array(count).keys()].join(", ").replace(/\d+/g, pattern);

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

// A GC-stress build collects every 8 instructions, so a guest callback per
// item, or evaluating a literal, costs its size times the live heap: these
// sizes take over half an hour there. A stress run evaluates a smaller literal
// and skips the 66,000-property one; the sources that only fail to compile are
// built by numbered and run in every build.
const gcStress = typeof Goccia !== "undefined" && Goccia.build.gcStress;
const distinctNames = gcStress ? 2000 : 20000;

describe("object literals and the constant limit", () => {
  test(`${distinctNames} distinct property names evaluate`, () => {
    expect(realm.evaluate(`const o = { ${numbered(distinctNames, "p$&: 0")} }; Object.keys(o).length`)).toBe(distinctNames);
  });

  test.runIf(!gcStress)("66,000 properties that repeat a few names evaluate", () => {
    expect(realm.evaluate(`const o = { ${list(66000, (i) => `p${i % 100}: ${i % 1000}`)} }; Object.keys(o).length + ":" + o.p99`)).toBe("100:999");
  });

  test("33,000 properties with distinct names and string values are a SyntaxError", () => {
    expectLimit(`({ ${numbered(33000, 'p$&: "v$&"')} })`, CONSTANT_LIMIT);
  });

  test("the SyntaxError for a literal inside a function can be caught around new Function", () => {
    let caught;
    try {
      new Function(`return () => ({ ${numbered(33000, 'p$&: "v$&"')} });`);
    } catch (error) {
      caught = error;
    }
    expect(caught instanceof SyntaxError).toBe(true);
    expect(caught.message.includes(CONSTANT_LIMIT)).toBe(true);
  });
});
