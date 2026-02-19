/*---
description: JSX shorthand props ({identifier} as attribute)
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX shorthand props", () => {
  test("shorthand prop passes variable by name", () => {
    const className = "active";
    const el = <div {className}></div>;
    expect(el.props.className).toBe("active");
  });

  test("shorthand prop with self-closing element", () => {
    const value = 42;
    const el = <input {value} />;
    expect(el.props.value).toBe(42);
  });

  test("mixed shorthand and regular props", () => {
    const id = "main";
    const el = <div {id} className="container" tabIndex={0}></div>;
    expect(el.props.id).toBe("main");
    expect(el.props.className).toBe("container");
    expect(el.props.tabIndex).toBe(0);
  });

  test("multiple shorthand props", () => {
    const id = "test";
    const className = "box";
    const el = <div {id} {className}></div>;
    expect(el.props.id).toBe("test");
    expect(el.props.className).toBe("box");
  });

  test("shorthand prop with children", () => {
    const title = "Hello";
    const el = <h1 {title}>content</h1>;
    expect(el.props.title).toBe("Hello");
    expect(el.children[0]).toBe("content");
  });
});
