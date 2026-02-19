/*---
description: JSX fragment support
features: [jsx]
---*/

const Fragment = Symbol("Fragment");
const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX fragments", () => {
  test("basic fragment", () => {
    const el = <>text</>;
    expect(el.tag).toBe(Fragment);
    expect(el.props).toBe(null);
    expect(el.children[0]).toBe("text");
  });

  test("fragment with multiple children", () => {
    const el = <><span>a</span><span>b</span></>;
    expect(el.tag).toBe(Fragment);
    expect(el.children.length).toBe(2);
    expect(el.children[0].tag).toBe("span");
    expect(el.children[1].tag).toBe("span");
  });

  test("fragment with mixed content", () => {
    const name = "World";
    const el = <>Hello {name}</>;
    expect(el.children[0]).toBe("Hello ");
    expect(el.children[1]).toBe("World");
  });

  test("empty fragment", () => {
    const el = <></>;
    expect(el.tag).toBe(Fragment);
    expect(el.children.length).toBe(0);
  });
});
