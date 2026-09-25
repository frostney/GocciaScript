/*---
description: Type arguments on call expressions are parsed and ignored at runtime
features: [types-as-comments]
---*/

describe("call expression type arguments", () => {

test("type arguments on an identifier callee", () => {
  const identity = (value) => value;

  expect(identity<string>("v")).toBe("v");
});

test("type arguments on a member-expression callee", () => {
  const holder = { fn: (value) => value };

  expect(holder.fn<number>(42)).toBe(42);
});

test("a function-typed type argument is erased, not evaluated", () => {
  const holder = { fn: (value) => value };
  const callback = () => Promise.resolve("x");

  expect(holder.fn<() => Promise<string>>(callback)).toBe(callback);
});

test("type arguments on a built-in method call", () => {
  expect(["a", "b"].map<string>((x) => x + "!")[1]).toBe("b!");
});

test("nested type arguments closing on '>>'", () => {
  const nested = ["a"].map<Map<string, number>>((x) => x);

  expect(nested[0]).toBe("a");
});

test("nested type arguments closing on '>>>'", () => {
  const nested = ["a"].map<Map<string, Set<Array<number>>>>((x) => x);

  expect(nested[0]).toBe("a");
});

test("union and intersection type arguments", () => {
  const identity = (value) => value;

  expect(identity<string | number>(1)).toBe(1);
  expect(identity<{ a: 1 } & { b: 2 }>("v")).toBe("v");
});

test("type arguments before a tagged template", () => {
  const tag = (parts) => "tagged:" + parts[0];

  expect(tag<string>`x`).toBe("tagged:x");
});

test("type arguments on a chained call", () => {
  const chain = { first: () => ({ second: (value) => value }) };

  expect(chain.first().second<string>("v")).toBe("v");
});

test("a comparison chain is not read as type arguments", () => {
  const a = 1;
  const b = 2;
  const c = false;

  expect(a < b > c).toBe(true);
});

test("less-than before a parenthesized operand stays relational", () => {
  const a = 1;
  const b = 2;

  expect(a < (b + 1)).toBe(true);
});

test("a logical operator disqualifies the type-argument reading", () => {
  const a = 1;
  const b = 2;
  const c = false;

  expect(a < b && c > (a)).toBe(false);
});

test("an arithmetic operator disqualifies the type-argument reading", () => {
  const a = 1;
  const b = 2;

  expect(a < b - 1).toBe(false);
});

});
