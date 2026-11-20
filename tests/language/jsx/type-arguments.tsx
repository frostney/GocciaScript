/*---
description: TypeScript type arguments on a JSX element are erased in .tsx
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

const Select = (props) => props;
const UI = { List: (props) => props };

describe("JSX type arguments in .tsx", () => {
  test("a self-closing element with type arguments", () => {
    const el = <Select<string> value="a" />;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ value: "a" });
    expect(el.children).toEqual([]);
  });

  test("a self-closing element with type arguments and no attributes", () => {
    const el = <Select<string> />;
    expect(el.tag).toBe(Select);
    expect(el.props).toBe(null);
  });

  test("an element with type arguments and children", () => {
    const el = <Select<number> size={2}>one<Select<boolean>></Select></Select>;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ size: 2 });
    expect(el.children[0]).toBe("one");
    expect(el.children[1].tag).toBe(Select);
    expect(el.children[1].props).toBe(null);
  });

  test("type arguments in a generic arrow's body", () => {
    const render = <T,>(value: T) => <Select<T> value={value} /* note */ />;
    expect(render(3).props).toEqual({ value: 3 });
  });

  test("nested, function, literal and union types are erased whole", () => {
    const el = <Select<Map<string, Array<() => void>>, -1, "a" | 'b', { k: [x: number] }> v={1} />;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ v: 1 });
  });

  test("template literal types are erased whole", () => {
    const string = "a comparison reading would use this";
    const el = <Select<`t${string}`, `a${`b${"}"}`}`> v={1} />;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ v: 1 });
  });

  test("an element with type arguments starts a statement after a line break", () => {
    const made = [];
    const record = (value) => made.push(value);
    // prettier-ignore
    if (made.length === 0)
      <Select<string> v={record(1)} />;
    // prettier-ignore
    if (made.length === 1) record(2);
    else
      <Select<string> v={record(3)} />;
    expect(made).toEqual([1, 2]);
    let el;
    // prettier-ignore
    if (made.length === 0) el = 0;
    else
      el = <Select<string> v={4} />;
    expect(el.props).toEqual({ v: 4 });
  });

  test("an element with type arguments starts a statement after a block", () => {
    const made = [];
    const record = (value) => made.push(value);
    // prettier-ignore
    const run = () => {
      if (made.length > 0) {}
      <Select<string> v={record(1)} />;
    };
    run();
    expect(made).toEqual([1]);
  });

  test("a list that opens with a generic function type", () => {
    const el = <Select<<T>(x: T) => T, '<<'> v={1} />;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ v: 1 });
  });

  test("comments and line breaks around the type arguments", () => {
    const el = (
      <Select /* before */ <
        // inside
        string
      > /* after */ v={1}
      />
    );
    expect(el.props).toEqual({ v: 1 });
  });

  test("a member tag with type arguments", () => {
    const el = <UI.List<string> items={[]} />;
    expect(el.tag).toBe(UI.List);
    expect(el.props).toEqual({ items: [] });
  });

  test("an element with type arguments inside an attribute expression", () => {
    const el = <Select icon={<Select<string> v={2} />} />;
    expect(el.props.icon.tag).toBe(Select);
    expect(el.props.icon.props).toEqual({ v: 2 });
  });
});
