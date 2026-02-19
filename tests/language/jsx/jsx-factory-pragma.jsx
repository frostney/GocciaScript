/* @jsxFactory h */
/* @jsxFragment Frag */
/*---
description: @jsxFactory and @jsxFragment pragmas to override createElement/Fragment
features: [jsx]
---*/

const h = (tag, props, ...children) => ({ tag, props, children });
const Frag = Symbol("Frag");

describe("@jsxFactory pragma", () => {
  test("uses custom factory function", () => {
    const el = <div>hello</div>;
    expect(el.tag).toBe("div");
    expect(el.children[0]).toBe("hello");
  });

  test("custom factory with attributes", () => {
    const el = <span id="test"></span>;
    expect(el.tag).toBe("span");
    expect(el.props.id).toBe("test");
  });

  test("custom factory with component", () => {
    const MyComp = (props) => props;
    const el = <MyComp value={1} />;
    expect(el.tag).toBe(MyComp);
    expect(el.props.value).toBe(1);
  });
});

describe("@jsxFragment pragma", () => {
  test("uses custom fragment symbol", () => {
    const el = <>content</>;
    expect(el.tag).toBe(Frag);
    expect(el.children[0]).toBe("content");
  });

  test("custom fragment with children", () => {
    const el = <><span>a</span><span>b</span></>;
    expect(el.tag).toBe(Frag);
    expect(el.children.length).toBe(2);
  });
});
