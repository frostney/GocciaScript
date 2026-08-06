/*---
description: Postfix non-null assertions are parsed and erased, leaving the member/call chain untouched
features: [types-as-comments]
---*/

describe("non-null assertions", () => {

test("assertion before a member access", () => {
  const nonNull = { v: "x" };

  expect(nonNull.v!.length).toBe(1);
});

test("assertion at the end of an expression", () => {
  const nonNull = { v: "x" };
  const bare = nonNull.v!;

  expect(bare).toBe("x");
});

test("assertion after a call", () => {
  const make = () => ({ x: 7 });

  expect(make()!.x).toBe(7);
});

test("assertion after an index access", () => {
  const arr = [{ y: 3 }];

  expect(arr[0]!.y).toBe(3);
});

test("chained assertions", () => {
  const deep = { a: { b: { c: 5 } } };

  expect(deep.a!.b!.c).toBe(5);
});

test("assertion on a parenthesized expression before a call", () => {
  const fn = () => "called";

  expect((fn)!()).toBe("called");
});

test("assertion combined with optional chaining", () => {
  const present = { a: { b: "deep" } };

  expect(present.a?.b!.length).toBe(4);
});

test("optional chain still short-circuits through an assertion", () => {
  const absent = { a: null };

  expect(absent.a?.b!.c).toBeUndefined();
});

test("assertion before postfix increment", () => {
  let counter = 1;
  const previous = counter!++;

  expect(previous).toBe(1);
  expect(counter).toBe(2);
});

test("assertion on an assignment target", () => {
  const target = { value: 0 };
  target!.value = 9;

  expect(target.value).toBe(9);
});

test("assertion before a call on the result", () => {
  const holder = { fn: () => "ran" };

  expect(holder.fn!()).toBe("ran");
});

test("strict inequality is not an assertion", () => {
  // '!==' and '!=' lex as single tokens, so they never reach the postfix
  // assertion branch. '!=' needs --compat-loose-equality and is covered in
  // tests/language/expressions/loose-equality/.
  const a = 1;
  const b = 2;

  expect(a !== b).toBe(true);
  expect(a !== 1).toBe(false);
});

test("prefix logical not is unaffected", () => {
  const truthy = 1;
  const nested = { v: "x" };

  expect(!truthy).toBe(false);
  expect(!nested.v!.length).toBe(false);
  expect(!!nested.v!).toBe(true);
});

test("assertion inside call arguments and array literals", () => {
  const nonNull = { v: "abc" };
  const lengths = [nonNull.v!.length];

  expect(lengths[0]).toBe(3);
  expect(Math.max(nonNull.v!.length, 1)).toBe(3);
});

});
