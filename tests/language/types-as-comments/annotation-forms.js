/*---
description: Strict types enforce an annotation only when every value its type admits passes the check, read from the type's outermost form
features: [types-as-comments, strict-type-enforcement]
---*/

describe("annotation forms that admit values outside a strict type", () => {

test("an indexed access parameter accepts the property's type", () => {
  type N = { kind: "text" | "row" };
  const f = (kind: N["kind"]) => kind;
  expect(f("text")).toBe("text");
});

test("an indexed access on an object type literal accepts a string", () => {
  const s: { a: string }["a"] = "s";
  expect(s).toBe("s");
});

test("a string literal type spelled like a keyword is not that keyword", () => {
  const n: "number" = "number";
  const o: "object" = "object";
  expect(n + o).toBe("numberobject");
});

test("a string literal type holding brackets is not an array or object type", () => {
  const brackets: "[]" = "[]";
  const braces: "{}" = "{}";
  expect(brackets + braces).toBe("[]{}");
});

test("a template literal type accepts a string", () => {
  const id: `id-${string}` = "id-1";
  expect(id).toBe("id-1");
});

test("keyof accepts a property key", () => {
  const key: keyof { a: 1 } = "a";
  expect(key).toBe("a");
});

test("a conditional type accepts a value of its branch", () => {
  const one: string extends string ? 1 : { a: 1 } = 1;
  expect(one).toBe(1);
});

test("a mapped type over a primitive accepts that primitive", () => {
  const s: Readonly<string> = "s";
  const t: Partial<string> = "t";
  expect(s + t).toBe("st");
});

test("a generic it does not know accepts a primitive", () => {
  const s: NonNullable<string> = "s";
  expect(s).toBe("s");
});

test("Object accepts a primitive", () => {
  const o: Object = 5;
  expect(o).toBe(5);
});

test("an object type literal accepts a primitive with its members", () => {
  const sized: { length: number } = "abc";
  expect(sized.length).toBe(3);
});

}); // describe

describe("annotation forms whose every value is an object", () => {

test("an array type rejects a primitive", () => {
  expect(() => { const xs: number[] = 5; }).toThrow(TypeError);
  expect(() => { const xs: readonly number[] = 5; }).toThrow(TypeError);
  expect(() => { const xs: (string | number)[] = 5; }).toThrow(TypeError);
});

test("a tuple type rejects a primitive", () => {
  expect(() => { const pair: [string, number] = "s"; }).toThrow(TypeError);
});

test("Array and the collection generics reject a primitive", () => {
  expect(() => { const xs: Array<number> = 5; }).toThrow(TypeError);
  expect(() => { const nested: Array<Array<number>> = 5; }).toThrow(TypeError);
  expect(() => { const m: Map<string, number> = 5; }).toThrow(TypeError);
  expect(() => { const p: Promise<number> = 5; }).toThrow(TypeError);
});

test("a function type rejects a primitive", () => {
  expect(() => { const f: () => void = 5; }).toThrow(TypeError);
});

test("an object-only form still accepts an object", () => {
  const xs: number[] = [1];
  const pair: [string, number] = ["a", 1];
  const m: Map<string, number> = new Map();
  const f: () => void = () => {};
  expect(xs.length + pair.length + m.size).toBe(3);
  expect(typeof f).toBe("function");
});

test("a parenthesized type is the type inside", () => {
  expect(() => { const n: (number) = "s"; }).toThrow(TypeError);
  const n: (number) = 1;
  expect(n).toBe(1);
});

}); // describe
