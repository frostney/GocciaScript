/*---
description: A call with more arguments than the bytecode encodes is a catchable SyntaxError
features: [ShadowRealm]
---*/

const ARGUMENT_LIMIT = "Too many arguments in one call: more than 65535";
const realm = new ShadowRealm();
const args = (count, item) => Array.from({ length: count }, () => item).join(", ");
const prelude = "const f = (...a) => a.length; const o = { m: (...a) => a.length }; class F { constructor(...a) { this.n = a.length; } } const t = (s, ...v) => v.length; const x = [1]; const k = 1;";

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

describe("call argument limit", () => {
  test("60,000 arguments are passed", () => {
    expect(realm.evaluate(`${prelude} f(${args(60000, "0")})`)).toBe(60000);
    expect(realm.evaluate(`${prelude} o.m(${args(60000, "0")})`)).toBe(60000);
    expect(realm.evaluate(`${prelude} new F(${args(60000, "0")}).n`)).toBe(60000);
  });

  test("70,000 arguments are a SyntaxError", () => {
    expectLimit(`${prelude} f(${args(70000, "0")})`, ARGUMENT_LIMIT);
    expectLimit(`${prelude} o.m(${args(70000, "0")})`, ARGUMENT_LIMIT);
    expectLimit(`${prelude} new F(${args(70000, "0")})`, ARGUMENT_LIMIT);
    expectLimit(`${prelude} f(${args(70000, "...x")})`, ARGUMENT_LIMIT);
  });

  test("a tagged template with 70,000 substitutions is a SyntaxError", () => {
    expectLimit(`${prelude} t\`${"${k}".repeat(70000)}\``,
      "Too many substitutions in one tagged template: more than 65534");
  });

  test("an untagged template with 70,000 substitutions evaluates", () => {
    expect(realm.evaluate(`${prelude} \`${"${k}".repeat(70000)}\`.length`)).toBe(70000);
  });
});
