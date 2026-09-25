/*---
description: Generic arrow function expressions parse in JSX-capable sources when the type parameter list is unambiguous
features: [types-as-comments]
---*/

describe("generic arrow functions", () => {

test("trailing-comma type parameter list", () => {
  const identity = <T,>(v: T): T => v;

  expect(identity("hi")).toBe("hi");
  expect(identity(3)).toBe(3);
});

test("default type parameter", () => {
  const withDefault = <T = string,>(v: T): T => v;

  expect(withDefault("hi")).toBe("hi");
});

test("multiple type parameters", () => {
  const pair = <A, B>(a: A, b: B): [A, B] => [a, b];
  const result = pair("x", 1);

  expect(result[0]).toBe("x");
  expect(result[1]).toBe(1);
});

test("block body and destructured parameters", () => {
  const sum = <T,>({ a, b }: { a: number, b: number }): number => {
    return a + b;
  };

  expect(sum({ a: 1, b: 2 })).toBe(3);
});

test("no-parameter generic arrow", () => {
  const make = <T,>(): string => "made";

  expect(make()).toBe("made");
});

test("generic arrow returning a generic arrow", () => {
  const outer = <T,>(v: T) => <U,>(u: U): T => v;

  expect(outer("kept")(1)).toBe("kept");
});

test("generic arrow as a call argument", () => {
  const apply = (fn, value) => fn(value);

  expect(apply(<T,>(v: T): T => v, "through")).toBe("through");
});

test("relational chains are unchanged", () => {
  const a = 1;
  const b = 2;
  const c = 3;

  expect(a < b > c).toBe(false);
  expect(a < b).toBe(true);
});

test("comparison followed by a parenthesized operand stays relational", () => {
  const a = 1;
  const b = 5;
  const c = 0;

  expect(a < b > (c)).toBe(true);
});

// The same source in a .ts file is a generic call — see
// call-expression-type-arguments.ts. Call-site type arguments are the one piece
// of type syntax GocciaScript does not erase in JavaScript sources, because the
// JavaScript reading is valid and means something else.
test("call-site type arguments are not type syntax in a .js source", () => {
  const identity = (value) => value;

  expect(() => identity<string>("v")).toThrow(ReferenceError);
});

});
