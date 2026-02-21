/*---
description: Edge cases for nested array method callbacks
features: [Array, map, filter, reduce, forEach, findIndex, indexOf]
---*/

describe("nested callbacks mutating shared state", () => {
  test("inner map does not corrupt outer map iteration", () => {
    const outer = [1, 2, 3];
    const inner = [10, 20, 30];
    const result = outer.map((o) => {
      const mapped = inner.map((i) => i + o);
      return mapped.reduce((a, b) => a + b, 0);
    });
    expect(result).toEqual([63, 66, 69]);
  });

  test("nested forEach with push to shared array", () => {
    const pairs = [];
    [1, 2].forEach((a) => {
      [3, 4].forEach((b) => {
        pairs.push([a, b]);
      });
    });
    expect(pairs).toEqual([[1, 3], [1, 4], [2, 3], [2, 4]]);
  });

  test("nested reduce building object", () => {
    const keys = ["a", "b"];
    const values = [1, 2, 3];
    const result = keys.reduce((obj, key) => {
      obj[key] = values.reduce((sum, v) => sum + v, 0);
      return obj;
    }, {});
    expect(result.a).toBe(6);
    expect(result.b).toBe(6);
  });
});

describe("nested callbacks with early termination", () => {
  test("find with nested some", () => {
    const data = [[1, 3], [2, 4], [5, 7]];
    const result = data.find((row) => row.some((x) => x === 4));
    expect(result).toEqual([2, 4]);
  });

  test("findIndex with nested every", () => {
    const data = [[1, 3, 5], [2, 4, 6], [7, 8, 9]];
    const idx = data.findIndex((row) => row.every((x) => x % 2 === 0));
    expect(idx).toBe(1);
  });

  test("some with nested find", () => {
    const matrix = [[1, 2], [3, 4], [5, 6]];
    const hasThree = matrix.some((row) => row.find((x) => x === 3) !== undefined);
    expect(hasThree).toBe(true);
  });

  test("every with nested includes", () => {
    const groups = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    const allHaveMultipleOf3 = groups.every((g) =>
      g.some((x) => x % 3 === 0)
    );
    expect(allHaveMultipleOf3).toBe(true);
  });
});

describe("nested callbacks preserving index independence", () => {
  test("inner and outer indices are independent", () => {
    const result = [10, 20].map((val, outerIdx) =>
      [1, 2, 3].map((x, innerIdx) => `${outerIdx}:${innerIdx}`)
    );
    expect(result[0]).toEqual(["0:0", "0:1", "0:2"]);
    expect(result[1]).toEqual(["1:0", "1:1", "1:2"]);
  });

  test("nested filter with independent predicates", () => {
    const data = [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]];
    const result = data.map((row, rowIdx) =>
      row.filter((val, colIdx) => colIdx > rowIdx)
    );
    expect(result[0]).toEqual([2, 3, 4, 5]);
    expect(result[1]).toEqual([8, 9, 10]);
  });
});

describe("nested callbacks with different array sizes", () => {
  test("map over empty inner arrays", () => {
    const data = [[], [1], [], [2, 3], []];
    const result = data.map((row) => row.map((x) => x * 2));
    expect(result).toEqual([[], [2], [], [4, 6], []]);
  });

  test("filter producing empty results inside map", () => {
    const data = [[1, 3, 5], [2, 4, 6], [7, 9, 11]];
    const result = data.map((row) => row.filter((x) => x % 2 === 0));
    expect(result).toEqual([[], [2, 4, 6], []]);
  });

  test("reduce on single-element arrays inside map", () => {
    const data = [[42], [99], [7]];
    const result = data.map((row) => row.reduce((a, b) => a + b, 0));
    expect(result).toEqual([42, 99, 7]);
  });
});

describe("deeply nested (4+ levels)", () => {
  test("4 levels of map nesting", () => {
    const data = [[[[1]]]];
    const result = data.map((a) =>
      a.map((b) =>
        b.map((c) =>
          c.map((d) => d + 1)
        )
      )
    );
    expect(result).toEqual([[[[2]]]]);
  });

  test("mixed methods at 4 levels", () => {
    const data = [[[1, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]]];
    const result = data.map((plane) =>
      plane.map((row) =>
        row.filter((x) => x % 2 === 0)
      ).filter((row) => row.length > 0)
    );
    expect(result).toEqual([[[2], [4, 6]], [[8], [10, 12]]]);
  });
});

describe("nested callbacks with method on result", () => {
  test("chained map().filter() inside outer map", () => {
    const data = [[1, 2, 3, 4], [5, 6, 7, 8]];
    const result = data.map((row) =>
      row.map((x) => x * 2).filter((x) => x > 5)
    );
    expect(result).toEqual([[6, 8], [10, 12, 14, 16]]);
  });

  test("chained filter().map() inside reduce", () => {
    const data = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    const result = data.reduce((acc, row) => {
      const processed = row.filter((x) => x > 2).map((x) => x * 10);
      return [...acc, ...processed];
    }, []);
    expect(result).toEqual([30, 40, 50, 60, 70, 80, 90]);
  });
});
