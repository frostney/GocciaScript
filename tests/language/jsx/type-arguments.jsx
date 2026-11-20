/*---
description: TypeScript type arguments on a JSX element are erased in .jsx, where a written generic call is still a comparison
features: [jsx]
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });

const Select = (props) => props;

describe("JSX type arguments in .jsx", () => {
  test("self-closing and opening tags with type arguments", () => {
    const el = <Select<string> value="a">one<Select<boolean> /></Select>;
    expect(el.tag).toBe(Select);
    expect(el.props).toEqual({ value: "a" });
    expect(el.children[0]).toBe("one");
    expect(el.children[1].tag).toBe(Select);
    expect(el.children[1].props).toBe(null);
  });

  test("types that read as expressions are still erased", () => {
    const el = <Select<-1, Map<string, () => void>, "a" | 'b'> /* note */ v={1} />;
    expect(el.props).toEqual({ v: 1 });
  });
});

describe("comparisons next to JSX in .jsx", () => {
  test("a written call-like comparison is still a comparison", () => {
    const a = 1;
    const b = 2;
    const c = 0;
    expect(a < b > (c)).toBe(true);
    expect(createElement < b > (c)).toBe(false);
  });

  test("a comparison chain that starts a line is not a tag", () => {
    const a = 1;
    const b = 2;
    const c = 3;
    const d = 0;
    // prettier-ignore
    const unclosed = a
      <b<c;
    // prettier-ignore
    const closed = a
      <b<c>d;
    const e = 1;
    const call = (callback) => callback();
    // prettier-ignore
    const divided = a
      <b<c>d / e;
    // prettier-ignore
    const nested = a
      <b<call(() => <Select<string> value={3} />);
    expect(unclosed).toBe(true);
    expect(closed).toBe(true);
    expect(divided).toBe(true);
    expect(nested).toBe(false);
  });

  test("a property named like a control-flow keyword still divides", () => {
    const options = { if: (value) => value, else: 8 };
    const quotient = { if: (8) / <Select />.v };
    expect(options.if(8) / 2).toBe(4);
    expect(options./* note */if(8) / 2).toBe(4);
    expect(options.else / 2).toBe(4);
    expect(quotient.if).toBeNaN();
  });

  test("a comparison chain on the line after an object literal is not a tag", () => {
    const b = 2;
    const c = 3;
    const d = 0;
    const e = 1;
    // prettier-ignore
    const result = {}
      <b<c>d / e;
    expect(result).toBe(true);
  });

  test("an element compared with `<` is not given type arguments", () => {
    const el = <Select />;
    expect(el < 3 > (1)).toBe(false);
    expect(<Select /> < 3 > (1)).toBe(false);
  });
});
