/*---
description: The JSX preprocessor reads `of` as a for-of keyword only in a for-of header
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

// An identifier named `of` divides; the '/' must not open a regex that runs on
// into the closing tag on the same line.
const of = 8;
const half = of / 2, halfElement = <b>{half}</b>;
const quarter =
  of / 4, quarterElement = <b>{quarter}</b>;

describe("contextual of in the JSX preprocessor", () => {
  test("an identifier named of divides at the top level", () => {
    expect(halfElement.tag).toBe("b");
    expect(halfElement.children[0]).toBe(4);
  });

  test("an identifier named of at the start of a line divides", () => {
    expect(quarterElement.tag).toBe("b");
    expect(quarterElement.children[0]).toBe(2);
  });

  test("of on its own line in a for-of header is followed by a regex", () => {
    const found = [];
    for (const tag
      of /<b>/.exec("<b>")) found.push(tag);
    const el = <i>{found.join("")}</i>;
    expect(el.children[0]).toBe("<b>");
  });

  test("a binding named of is followed by the keyword and a regex", () => {
    const found = [];
    for (const of of /<i>/.exec("<i>")) found.push(of);
    const el = <b>{found.join("")}</b>;
    expect(el.children[0]).toBe("<i>");
  });
});
