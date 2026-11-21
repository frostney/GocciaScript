/* @jsxFactory lib.default */
/*---
description: TypeScript type arguments on a JSX element are erased when a @jsxFactory pragma names a member factory
features: [jsx]
---*/

const lib = { default: (tag, props, ...children) => ({ tag, props, children }) };
const Select = (props) => props;
const string = "a value that a comparison reading would use";

describe("JSX type arguments with a @jsxFactory pragma", () => {
  test("a member factory ending in a reserved word still takes the type arguments", () => {
    const el = <Select<string> value={1} />;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ value: 1 });
  });
});
