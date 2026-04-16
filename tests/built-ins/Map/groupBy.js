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

  test("accepts a Set", () => {
    const result = Map.groupBy(new Set([1, 2, 3, 4]), (n) => n % 2 === 0 ? "even" : "odd");
    expect(result.get("odd")).toEqual([1, 3]);
    expect(result.get("even")).toEqual([2, 4]);
  });

  test("accepts a Map", () => {
    const source = new Map([["a", 1], ["b", 2], ["c", 3]]);
    const result = Map.groupBy(source, ([key, value]) => value > 1 ? "big" : "small");
    expect(result.get("small")).toEqual([["a", 1]]);
    expect(result.get("big")).toEqual([["b", 2], ["c", 3]]);
  });

  test("accepts a string", () => {
    const result = Map.groupBy("aabbc", (ch) => ch);
    expect(result.get("a")).toEqual(["a", "a"]);
    expect(result.get("b")).toEqual(["b", "b"]);
    expect(result.get("c")).toEqual(["c"]);
  });

  test("accepts a custom iterable with Symbol.iterator", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        const values = [5, 10, 15, 20];
        return {
          next() {
            if (i < values.length) {
              const val = values[i];
              i = i + 1;
              return { value: val, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    const result = Map.groupBy(iterable, (n) => n >= 15 ? "high" : "low");
    expect(result.get("low")).toEqual([5, 10]);
    expect(result.get("high")).toEqual([15, 20]);
  });

  test("callback receives correct index for iterables", () => {
    const indices = [];
    Map.groupBy(new Set(["x", "y", "z"]), (el, idx) => {
      indices.push(idx);
      return "group";
    });
    expect(indices).toEqual([0, 1, 2]);
  });

  test("throws TypeError for non-iterable", () => {
    expect(() => Map.groupBy(42, () => "a")).toThrow(TypeError);
    expect(() => Map.groupBy(true, () => "a")).toThrow(TypeError);
    expect(() => Map.groupBy(null, () => "a")).toThrow(TypeError);
  });

  test("empty iterable returns empty Map", () => {
    const result = Map.groupBy(new Set(), () => "a");
    expect(result.size).toBe(0);
  });
});
