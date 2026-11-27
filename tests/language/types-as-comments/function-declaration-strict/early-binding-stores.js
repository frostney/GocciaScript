/*---
description: Stores in hoisted or suspended functions enforce the type of a binding declared later
features: [types-as-comments, strict-type-enforcement, function-declarations]
---*/

describe("typed captures in hoisted and suspended functions", () => {
  test("a hoisted function checks its captured binding", () => {
    let value: string = "original";
    function assign() { value = 5; }
    expect(assign).toThrow(TypeError);
    expect(value).toBe("original");
  });

  test("a generator checks an assignment after yield", () => {
    function* assign() {
      yield value;
      value = 5;
    }
    let value: string = "original";
    const iterator = assign();
    expect(iterator.next().value).toBe("original");
    expect(() => iterator.next()).toThrow(TypeError);
    expect(value).toBe("original");
  });
});
