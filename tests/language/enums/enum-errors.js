/*---
description: Enum members must have Number, String, or Symbol values
features: [enum-declaration]
---*/

test("rejects boolean value", () => {
  expect(() => {
    enum E { A = true }
  }).toThrow();
});

test("rejects null value", () => {
  expect(() => {
    enum E { A = null }
  }).toThrow();
});

test("rejects undefined value", () => {
  expect(() => {
    enum E { A = undefined }
  }).toThrow();
});

test("rejects object value", () => {
  expect(() => {
    enum E { A = {} }
  }).toThrow();
});

test("rejects array value", () => {
  expect(() => {
    enum E { A = [] }
  }).toThrow();
});

test("rejects function value", () => {
  expect(() => {
    enum E { A = () => {} }
  }).toThrow();
});
