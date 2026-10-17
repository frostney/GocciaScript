/*---
description: Binding patterns accept the contextual words `from` and `as` as BindingIdentifiers (ES2026 §13.1, §14.3.3)
features: [destructuring]
---*/

describe("contextual words as pattern bindings", () => {
  test("array pattern elements", () => {
    const [from] = [1];
    const [as] = [2];
    expect(from).toBe(1);
    expect(as).toBe(2);
  });

  test("array pattern elements with defaults", () => {
    const [from = 3, as = 4] = [];
    expect(from).toBe(3);
    expect(as).toBe(4);
  });

  test("rest elements", () => {
    const [...from] = [1, 2];
    const [head, ...as] = [1, 2, 3];
    expect(from).toEqual([1, 2]);
    expect(head).toBe(1);
    expect(as).toEqual([2, 3]);
  });

  test("object pattern property targets", () => {
    const { a: from } = { a: 1 };
    const { b: as = 5 } = {};
    expect(from).toBe(1);
    expect(as).toBe(5);
  });

  test("object pattern shorthand", () => {
    const { from, as } = { from: 1, as: 2 };
    expect(from).toBe(1);
    expect(as).toBe(2);
  });

  test("nested patterns", () => {
    const { range: [from, as] } = { range: [1, 2] };
    expect(from).toBe(1);
    expect(as).toBe(2);
  });

  test("let declarations", () => {
    let [from, as] = [1, 2];
    [from, as] = [as, from];
    expect(from).toBe(2);
    expect(as).toBe(1);
  });

  test("arrow function parameters", () => {
    const first = ([from]) => from;
    const second = ({ a: as }) => as;
    const rest = (...[from, as]) => from + as;
    expect(first([1])).toBe(1);
    expect(second({ a: 2 })).toBe(2);
    expect(rest(1, 2)).toBe(3);
  });

  test("for...of heads", () => {
    const seen = [];
    for (const [from, as] of [[1, 2], [3, 4]]) {
      seen.push(from + as);
    }
    expect(seen).toEqual([3, 7]);
  });

  test("catch parameters", () => {
    let caught;
    try {
      throw [1, { a: 2 }];
    } catch ([from, { a: as }]) {
      caught = from + as;
    }
    expect(caught).toBe(3);
  });
});
