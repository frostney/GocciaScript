/*---
description: Hoisted function declarations capture initialized imported bindings
features: [modules, compat-function]
---*/

import { words, words as nonHoistedWords } from "./helpers/words.js";

const arrow = (value) => words(value);
const fnExpression = function (value) {
  return words(value);
};

const nonHoistedArrow = (value) => nonHoistedWords(value);
const nonHoistedExpression = function (value) {
  return nonHoistedWords(value);
};

function fnDeclaration(value) {
  return words(value);
}

describe("hoisted function declarations capture imported bindings", () => {
  test("function declarations read imports after module evaluation", () => {
    expect(arrow("a b")).toBe(2);
    expect(fnExpression("a b c")).toBe(3);
    expect(fnDeclaration("a b c d")).toBe(4);
  });

  test("non-hoisted closures capture imports without a hoisted declaration", () => {
    expect(nonHoistedArrow("a b c d e")).toBe(5);
    expect(nonHoistedExpression("a b c d e f")).toBe(6);
  });
});
