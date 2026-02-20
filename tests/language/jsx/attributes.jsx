/*---
description: JSX attribute parsing including string, expression, boolean, and spread
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX attributes", () => {
  test("string attribute with double quotes", () => {
    const el = <div className="foo"></div>;
    expect(el.props.className).toBe("foo");
  });

  test("string attribute with single quotes", () => {
    const el = <div className='bar'></div>;
    expect(el.props.className).toBe("bar");
  });

  test("expression attribute", () => {
    const x = 42;
    const el = <div count={x}></div>;
    expect(el.props.count).toBe(42);
  });

  test("computed expression attribute", () => {
    const el = <div count={1 + 2}></div>;
    expect(el.props.count).toBe(3);
  });

  test("boolean attribute (no value)", () => {
    const el = <input disabled />;
    expect(el.props.disabled).toBe(true);
  });

  test("multiple attributes", () => {
    const el = <div id="main" className="active" tabIndex={0}></div>;
    expect(el.props.id).toBe("main");
    expect(el.props.className).toBe("active");
    expect(el.props.tabIndex).toBe(0);
  });

  test("spread attributes", () => {
    const props = { id: "spread", className: "test" };
    const el = <div {...props}></div>;
    expect(el.props.id).toBe("spread");
    expect(el.props.className).toBe("test");
  });

  test("spread with normal attributes", () => {
    const base = { className: "base" };
    const el = <div {...base} id="override"></div>;
    expect(el.props.className).toBe("base");
    expect(el.props.id).toBe("override");
  });

  test("hyphenated attribute names", () => {
    const el = <div data-testid="foo"></div>;
    expect(el.props["data-testid"]).toBe("foo");
  });

  test("undefined variable in attribute expression throws", () => {
    expect(() => {
      const el = <div value={nonExistentAttrVar}></div>;
    }).toThrow(ReferenceError);
  });
});
