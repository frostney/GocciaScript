/*---
description: A class body past a function's constant or function limit is a catchable SyntaxError
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

describe("class body limits", () => {
  test("65,600 distinct field names are a SyntaxError", () => {
    expectLimit(`typeof (class { ${list(65600, (i) => `f${i};`, " ")} })`,
      "Too many constants in one function: more than 65535");
  });

  test("65,537 methods of one name are a SyntaxError", () => {
    expectLimit(`typeof (class { ${list(65537, () => "m() {}", " ")} })`,
      "Too many functions defined directly in one function: more than 65536");
  });
});
