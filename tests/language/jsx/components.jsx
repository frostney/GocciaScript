/*---
description: JSX component elements (uppercase tags pass identifier, not string)
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX components", () => {
  test("uppercase tag passes identifier reference", () => {
    const MyComponent = (props) => props;
    const el = <MyComponent />;
    expect(el.tag).toBe(MyComponent);
    expect(typeof el.tag).toBe("function");
  });

  test("component with props", () => {
    const Button = (props) => props;
    const el = <Button label="Click me" size={3} />;
    expect(el.tag).toBe(Button);
    expect(el.props.label).toBe("Click me");
    expect(el.props.size).toBe(3);
  });

  test("component with children", () => {
    const Container = (props) => props;
    const el = <Container><span>child</span></Container>;
    expect(el.tag).toBe(Container);
    expect(el.children[0].tag).toBe("span");
  });

  test("dotted component name", () => {
    const UI = { Button: (props) => props };
    const el = <UI.Button label="ok" />;
    expect(el.tag).toBe(UI.Button);
    expect(el.props.label).toBe("ok");
  });
});
