/*---
description: An array literal has no element limit of its own; past a function's constant limit it is a catchable SyntaxError
features: [ShadowRealm]
---*/

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

describe("array literals and the constant limit", () => {
  test("66,000 repeated elements evaluate", () => {
    expect(realm.evaluate(`[${list(66000, () => "0")}].length`)).toBe(66000);
  });

  test("65,600 distinct strings are a SyntaxError", () => {
    expectLimit(`[${list(65600, (i) => `"s${i}"`)}].length`,
      "Too many constants in one function: more than 65535");
  });
});
