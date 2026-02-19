/*---
description: JSX children including text, expressions, and nested elements
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX children", () => {
  test("text content", () => {
    const el = <p>Hello World</p>;
    expect(el.children[0]).toBe("Hello World");
  });

  test("expression child", () => {
    const name = "Alice";
    const el = <p>{name}</p>;
    expect(el.children[0]).toBe("Alice");
  });

  test("mixed text and expression", () => {
    const name = "Bob";
    const el = <p>Hello {name}</p>;
    expect(el.children.length).toBe(2);
    expect(el.children[0]).toBe("Hello ");
    expect(el.children[1]).toBe("Bob");
  });

  test("multiple expression children", () => {
    const a = 1;
    const b = 2;
    const el = <p>{a} and {b}</p>;
    expect(el.children[0]).toBe(1);
    expect(el.children[1]).toBe(" and ");
    expect(el.children[2]).toBe(2);
  });

  test("nested element child", () => {
    const el = <div><span>inner</span></div>;
    expect(el.tag).toBe("div");
    expect(el.children[0].tag).toBe("span");
    expect(el.children[0].children[0]).toBe("inner");
  });

  test("multiple nested children", () => {
    const el = <ul><li>one</li><li>two</li></ul>;
    expect(el.children.length).toBe(2);
    expect(el.children[0].tag).toBe("li");
    expect(el.children[1].tag).toBe("li");
    expect(el.children[0].children[0]).toBe("one");
    expect(el.children[1].children[0]).toBe("two");
  });

  test("expression with object literal", () => {
    const el = <div>{{ key: "value" }}</div>;
    expect(el.children[0].key).toBe("value");
  });

  test("expression with arrow function", () => {
    const el = <div>{() => 42}</div>;
    expect(typeof el.children[0]).toBe("function");
    expect(el.children[0]()).toBe(42);
  });

  test("expression with string containing braces", () => {
    const el = <div>{"hello {world}"}</div>;
    expect(el.children[0]).toBe("hello {world}");
  });
});
