// @jsxFactory h
// @jsxFragment F
/*---
description: @jsxFactory/@jsxFragment via single-line comments
features: [jsx]
---*/

const h = (tag, props, ...children) => ({ tag, props, children });
const F = Symbol("F");

describe("@jsxFactory via line comment", () => {
  test("uses custom factory from line comment pragma", () => {
    const el = <div className="test">hello</div>;
    expect(el.tag).toBe("div");
    expect(el.props.className).toBe("test");
    expect(el.children[0]).toBe("hello");
  });

  test("uses custom fragment from line comment pragma", () => {
    const el = <>world</>;
    expect(el.tag).toBe(F);
    expect(el.children[0]).toBe("world");
  });
});
