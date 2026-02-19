/*---
description: Basic JSX element parsing and transformation to createElement calls
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("basic JSX elements", () => {
  test("simple element with text child", () => {
    const el = <div>hello</div>;
    expect(el.tag).toBe("div");
    expect(el.props).toBe(null);
    expect(el.children[0]).toBe("hello");
  });

  test("self-closing element", () => {
    const el = <br />;
    expect(el.tag).toBe("br");
    expect(el.props).toBe(null);
    expect(el.children.length).toBe(0);
  });

  test("self-closing element without space", () => {
    const el = <hr/>;
    expect(el.tag).toBe("hr");
    expect(el.props).toBe(null);
  });

  test("element with no children", () => {
    const el = <div></div>;
    expect(el.tag).toBe("div");
    expect(el.children.length).toBe(0);
  });

  test("lowercase tag produces string tag name", () => {
    const el = <span>text</span>;
    expect(typeof el.tag).toBe("string");
    expect(el.tag).toBe("span");
  });
});
