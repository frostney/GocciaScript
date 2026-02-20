describe("Object.groupBy", () => {
  test("groups array elements by callback result", () => {
    const result = Object.groupBy([1, 2, 3, 4, 5], (n) => n % 2 === 0 ? "even" : "odd");
    expect(result.odd).toEqual([1, 3, 5]);
    expect(result.even).toEqual([2, 4]);
  });

  test("empty array returns empty object", () => {
    const result = Object.groupBy([], () => "a");
    expect(Object.keys(result).length).toBe(0);
  });

  test("all items in same group", () => {
    const result = Object.groupBy([1, 2, 3], () => "all");
    expect(result.all).toEqual([1, 2, 3]);
  });

  test("callback receives element and index", () => {
    const indices = [];
    Object.groupBy(["a", "b"], (el, idx) => {
      indices.push(idx);
      return "group";
    });
    expect(indices).toEqual([0, 1]);
  });

  test("result has null prototype", () => {
    const result = Object.groupBy([1], () => "a");
    expect(Object.getPrototypeOf(result)).toBe(null);
  });
});
