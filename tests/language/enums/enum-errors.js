/*---
description: Enum members must have Number, String, or Symbol values
features: [enum-declaration]
---*/

test("rejects boolean value", () => {
  expect(() => {
    enum E { A = true }
  }).toThrow(TypeError);
});

test("rejects null value", () => {
  expect(() => {
    enum E { A = null }
  }).toThrow(TypeError);
});

test("rejects undefined value", () => {
  expect(() => {
    enum E { A = undefined }
  }).toThrow(TypeError);
});

test("rejects object value", () => {
  expect(() => {
    enum E { A = {} }
  }).toThrow(TypeError);
});

test("rejects array value", () => {
  expect(() => {
    enum E { A = [] }
  }).toThrow(TypeError);
});

test("rejects function value", () => {
  expect(() => {
    enum E { A = () => {} }
  }).toThrow(TypeError);
});
