/*---
description: The object annotation accepts any reference type but rejects primitives
features: [types-as-comments, strict-type-enforcement]
---*/

describe("reference type enforcement", () => {

test("object annotation accepts plain object", () => {
  let o: object = { a: 1 };
  expect(o.a).toBe(1);
});

test("object annotation accepts array", () => {
  let o: object = [1, 2, 3];
  expect(o.length).toBe(3);
});

test("object annotation accepts reassignment to different reference types", () => {
  let o: object = { x: 1 };
  o = [1, 2];
  expect(o.length).toBe(2);
  o = new Map();
  expect(o instanceof Map).toBe(true);
});

test("object annotation rejects number", () => {
  let o: object = {};
  expect(() => { o = 42; }).toThrow(TypeError);
});

test("object annotation rejects string", () => {
  let o: object = {};
  expect(() => { o = "hello"; }).toThrow(TypeError);
});

test("object annotation rejects boolean", () => {
  let o: object = {};
  expect(() => { o = true; }).toThrow(TypeError);
});

test("object annotation rejects null", () => {
  let o: object = {};
  expect(() => { o = null; }).toThrow(TypeError);
});

test("object annotation rejects undefined", () => {
  let o: object = {};
  expect(() => { o = undefined; }).toThrow(TypeError);
});

}); // describe
