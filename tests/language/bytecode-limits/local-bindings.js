/*---
description: A function with more local bindings than the bytecode encodes is a catchable SyntaxError, also inside a class element
features: [ShadowRealm]
---*/

const LOCAL_LIMIT = "Too many local bindings in one function: more than 65535";
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

const lets = list(65600, (i) => `let v${i} = 0;`, " ");

describe("local binding limit", () => {
  test("65,600 local bindings in an arrow are a SyntaxError", () => {
    expectLimit(`(() => { ${lets} return 1; })()`, LOCAL_LIMIT);
  });

  test("65,600 local bindings in a class method are a SyntaxError", () => {
    expectLimit(`typeof (class { m() { ${lets} return 1; } })`, LOCAL_LIMIT);
  });
});
