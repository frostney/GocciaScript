describe("Map.groupBy", () => {
  test("groups array elements by callback result", () => {
    const result = Map.groupBy([1, 2, 3, 4], (n) => n % 2 === 0 ? "even" : "odd");
    expect(result.get("odd")).toEqual([1, 3]);
    expect(result.get("even")).toEqual([2, 4]);
  });

  test("returns a Map", () => {
    const result = Map.groupBy([1], () => "a");
    expect(result.size).toBe(1);
  });

  test("empty array returns empty Map", () => {
    const result = Map.groupBy([], () => "a");
    expect(result.size).toBe(0);
  });

  test("all items in same group", () => {
    const result = Map.groupBy([1, 2, 3], () => "all");
    expect(result.get("all")).toEqual([1, 2, 3]);
  });

  test("callback receives element and index", () => {
    const indices = [];
    Map.groupBy(["a", "b"], (el, idx) => {
      indices.push(idx);
      return "group";
    });
    expect(indices).toEqual([0, 1]);
  });
});
