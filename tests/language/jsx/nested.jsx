/*---
description: Deeply nested JSX structures and mixed children
features: [jsx]
---*/

const Fragment = Symbol("Fragment");
const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("nested JSX", () => {
  test("deeply nested elements", () => {
    const el = <div><section><article><p>deep</p></article></section></div>;
    expect(el.tag).toBe("div");
    expect(el.children[0].tag).toBe("section");
    expect(el.children[0].children[0].tag).toBe("article");
    expect(el.children[0].children[0].children[0].tag).toBe("p");
    expect(el.children[0].children[0].children[0].children[0]).toBe("deep");
  });

  test("siblings and nesting combined", () => {
    const el = (
      <div>
        <h1>Title</h1>
        <p>Paragraph</p>
      </div>
    );
    expect(el.tag).toBe("div");
    expect(el.children[0].tag).toBe("h1");
    expect(el.children[0].children[0]).toBe("Title");
    expect(el.children[1].tag).toBe("p");
    expect(el.children[1].children[0]).toBe("Paragraph");
  });

  test("JSX as expression in attribute", () => {
    const el = <div content={<span>inner</span>}></div>;
    expect(el.props.content.tag).toBe("span");
    expect(el.props.content.children[0]).toBe("inner");
  });

  test("JSX in ternary expression", () => {
    const show = true;
    const el = show ? <div>yes</div> : <span>no</span>;
    expect(el.tag).toBe("div");
    expect(el.children[0]).toBe("yes");
  });

  test("JSX in array", () => {
    const items = [<li>one</li>, <li>two</li>];
    expect(items[0].tag).toBe("li");
    expect(items[1].tag).toBe("li");
  });

  test("self-closing elements mixed with open elements", () => {
    const el = <div><br /><span>text</span><hr /></div>;
    expect(el.children.length).toBe(3);
    expect(el.children[0].tag).toBe("br");
    expect(el.children[1].tag).toBe("span");
    expect(el.children[2].tag).toBe("hr");
  });
});
