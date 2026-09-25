/*---
description: An identifier named `of` that starts a statement under ASI is not the for-of keyword
features: [automatic-semicolon-insertion, jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

// The line before ends an expression, as a for-of binding would, but this is
// no for-of header: `of` is the identifier and the '/' divides.
const of = 8
let result = 0
let element = null
result = 1
of / 2, element = <b>{of / 4}</b>

describe("ASI and contextual of in the JSX preprocessor", () => {
  test("an of that starts a statement divides", () => {
    expect(element.tag).toBe("b");
    expect(element.children[0]).toBe(2);
  });
});
