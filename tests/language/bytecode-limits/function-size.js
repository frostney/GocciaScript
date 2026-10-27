/*---
description: A function with more constants, functions or local bindings than the bytecode encodes is a catchable SyntaxError
features: [ShadowRealm]
---*/

const realm = new ShadowRealm();
const list = (count, item, separator = " ") =>
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

describe("function size limits", () => {
  test("an array of 70,000 distinct strings is a SyntaxError", () => {
    expectLimit(`[${list(70000, (i) => `"s${i}"`, ", ")}].length`,
      "Too many constants in one function: more than 65535");
  });

  test("an array of 70,000 repeated values evaluates", () => {
    expect(realm.evaluate(`[${list(70000, () => "0", ", ")}].length`)).toBe(70000);
  });

  test("a class with 70,000 distinct method names is a SyntaxError", () => {
    expectLimit(`typeof (class { ${list(70000, (i) => `m${i}() {}`)} })`,
      "Too many constants in one function: more than 65535");
  });

  test("a class with 70,000 methods of one name is a SyntaxError", () => {
    expectLimit(`typeof (class { ${list(70000, () => "m() {}")} })`,
      "Too many functions defined directly in one function: more than 65536");
  });

  test("70,000 local bindings in one function are a SyntaxError", () => {
    expectLimit(`(() => { ${list(70000, (i) => `let v${i} = 0;`)} return 1; })()`,
      "Too many local bindings in one function: more than 65535");
  });

  test("a limit inside a class method, getter or field is a SyntaxError", () => {
    const lets = list(70000, (i) => `let v${i} = 0;`);
    const keys = list(70000, (i) => `p${i}: 0`, ", ");
    const message = "Too many local bindings in one function: more than 65535";
    expectLimit(`typeof (class { m() { ${lets} } })`, message);
    expectLimit(`typeof (class { get g() { ${lets} return 1; } })`, message);
    expectLimit(`typeof (class { f = (() => ({ ${keys} }))(); })`,
      "Too many constants in one function: more than 65535");
  });

  test("60,000 local bindings in one function evaluate", () => {
    expect(realm.evaluate(`(() => { ${list(60000, (i) => `let v${i} = ${i % 10};`)} return v59999; })()`)).toBe(9);
  });
});
