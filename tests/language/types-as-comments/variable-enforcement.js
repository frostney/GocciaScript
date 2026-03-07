/*---
description: Type annotations on variables enforce type constraints at runtime (bytecode mode)
features: [types-as-comments, strict-type-enforcement]
---*/

describe.skipIf(!GocciaScript.strictTypes)("variable type enforcement", () => {

test("number annotation rejects string assignment", () => {
  let x: number = 5;
  expect(() => { x = "hello"; }).toThrow(TypeError);
});

test("number annotation rejects boolean assignment", () => {
  let x: number = 10;
  expect(() => { x = true; }).toThrow(TypeError);
});

test("number annotation rejects null assignment", () => {
  let x: number = 1;
  expect(() => { x = null; }).toThrow(TypeError);
});

test("number annotation rejects undefined assignment", () => {
  let x: number = 1;
  expect(() => { x = undefined; }).toThrow(TypeError);
});

test("number annotation rejects object assignment", () => {
  let x: number = 1;
  expect(() => { x = {}; }).toThrow(TypeError);
});

test("string annotation rejects number assignment", () => {
  let s: string = "hello";
  expect(() => { s = 42; }).toThrow(TypeError);
});

test("string annotation rejects boolean assignment", () => {
  let s: string = "hello";
  expect(() => { s = false; }).toThrow(TypeError);
});

test("boolean annotation rejects number assignment", () => {
  let b: boolean = true;
  expect(() => { b = 0; }).toThrow(TypeError);
});

test("boolean annotation rejects string assignment", () => {
  let b: boolean = false;
  expect(() => { b = "yes"; }).toThrow(TypeError);
});

test("initial assignment with wrong type throws", () => {
  expect(() => { let x: number = "wrong"; }).toThrow(TypeError);
});

test("initial assignment with correct type works", () => {
  let x: number = 42;
  expect(x).toBe(42);

  let s: string = "hello";
  expect(s).toBe("hello");

  let b: boolean = true;
  expect(b).toBe(true);
});

test("reassignment with correct type works", () => {
  let x: number = 5;
  x = 10;
  expect(x).toBe(10);
  x = -3;
  expect(x).toBe(-3);

  let s: string = "a";
  s = "b";
  expect(s).toBe("b");

  let b: boolean = true;
  b = false;
  expect(b).toBe(false);
});

test("error message includes type names", () => {
  let x: number = 1;
  try {
    x = "hello";
  } catch (e) {
    expect(e instanceof TypeError).toBe(true);
    expect(e.message).toBe("Type 'string' is not assignable to type 'number'");
  }
});

}); // describe
