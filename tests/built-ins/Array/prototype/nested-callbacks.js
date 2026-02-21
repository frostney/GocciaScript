/*---
description: Nested array method callbacks (map/filter/reduce inside map/filter/reduce)
features: [Array, map, filter, reduce, forEach, find, some, every, flatMap]
---*/

describe("nested map", () => {
  test("map inside map", () => {
    const matrix = [[1, 2], [3, 4], [5, 6]];
    const doubled = matrix.map((row) => row.map((x) => x * 2));
    expect(doubled).toEqual([[2, 4], [6, 8], [10, 12]]);
  });

  test("map inside map with index", () => {
    const matrix = [[1, 2, 3], [4, 5, 6]];
    const result = matrix.map((row, rowIdx) =>
      row.map((val, colIdx) => `${rowIdx},${colIdx}:${val}`)
    );
    expect(result[0][0]).toBe("0,0:1");
    expect(result[1][2]).toBe("1,2:6");
  });

  test("triple nested map", () => {
    const cube = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]];
    const result = cube.map((plane) =>
      plane.map((row) =>
        row.map((x) => x * 10)
      )
    );
    expect(result).toEqual([[[10, 20], [30, 40]], [[50, 60], [70, 80]]]);
  });
});

describe("nested filter", () => {
  test("filter inside map", () => {
    const data = [[1, 2, 3, 4], [5, 6, 7, 8]];
    const evens = data.map((row) => row.filter((x) => x % 2 === 0));
    expect(evens).toEqual([[2, 4], [6, 8]]);
  });

  test("map inside filter", () => {
    const matrix = [[1, 2], [3, 4], [5, 6]];
    const result = matrix
      .filter((row) => row.map((x) => x * 2).reduce((a, b) => a + b, 0) > 10)
      .map((row) => row.reduce((a, b) => a + b, 0));
    expect(result).toEqual([7, 11]);
  });

  test("filter inside filter", () => {
    const groups = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    const result = groups
      .filter((group) => group.filter((x) => x % 2 === 0).length > 0)
      .map((group) => group.filter((x) => x % 2 === 0));
    expect(result).toEqual([[2], [4, 6], [8]]);
  });
});

describe("nested reduce", () => {
  test("reduce inside reduce (flatten)", () => {
    const matrix = [[1, 2], [3, 4], [5, 6]];
    const flat = matrix.reduce((acc, row) =>
      row.reduce((inner, val) => [...inner, val], acc)
    , []);
    expect(flat).toEqual([1, 2, 3, 4, 5, 6]);
  });

  test("reduce with map inside", () => {
    const data = [[1, 2, 3], [4, 5, 6]];
    const result = data.reduce((acc, row) => {
      const doubled = row.map((x) => x * 2);
      return [...acc, ...doubled];
    }, []);
    expect(result).toEqual([2, 4, 6, 8, 10, 12]);
  });

  test("reduce inside map", () => {
    const matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    const sums = matrix.map((row) => row.reduce((a, b) => a + b, 0));
    expect(sums).toEqual([6, 15, 24]);
  });
});

describe("nested forEach", () => {
  test("forEach inside forEach", () => {
    const matrix = [[1, 2], [3, 4]];
    const collected = [];
    matrix.forEach((row) => {
      row.forEach((val) => {
        collected.push(val);
      });
    });
    expect(collected).toEqual([1, 2, 3, 4]);
  });

  test("forEach inside map", () => {
    const data = [[1, 2, 3], [4, 5, 6]];
    const result = data.map((row) => {
      let sum = 0;
      row.forEach((x) => { sum = sum + x; });
      return sum;
    });
    expect(result).toEqual([6, 15]);
  });
});

describe("nested find/some/every", () => {
  test("find inside map", () => {
    const data = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    const firstEvens = data.map((row) => row.find((x) => x % 2 === 0));
    expect(firstEvens).toEqual([2, 4, 8]);
  });

  test("some inside filter", () => {
    const groups = [[1, 3, 5], [2, 4, 6], [7, 9, 11]];
    const hasEven = groups.filter((g) => g.some((x) => x % 2 === 0));
    expect(hasEven).toEqual([[2, 4, 6]]);
  });

  test("every inside map", () => {
    const groups = [[2, 4, 6], [1, 3, 5], [2, 3, 4]];
    const allEven = groups.map((g) => g.every((x) => x % 2 === 0));
    expect(allEven).toEqual([true, false, false]);
  });
});

describe("nested flatMap", () => {
  test("flatMap with inner map", () => {
    const data = [[1, 2], [3, 4]];
    const result = data.flatMap((row) => row.map((x) => x * 10));
    expect(result).toEqual([10, 20, 30, 40]);
  });

  test("flatMap with inner filter", () => {
    const data = [[1, 2, 3], [4, 5, 6]];
    const result = data.flatMap((row) => row.filter((x) => x % 2 === 0));
    expect(result).toEqual([2, 4, 6]);
  });
});

describe("complex nesting with closures", () => {
  test("outer variable captured in nested callbacks", () => {
    const multipliers = [2, 3, 4];
    const data = [1, 2, 3];
    const result = multipliers.map((m) =>
      data.map((d) => d * m)
    );
    expect(result).toEqual([[2, 4, 6], [3, 6, 9], [4, 8, 12]]);
  });

  test("accumulator shared across nested iterations", () => {
    const matrix = [[1, 2], [3, 4], [5, 6]];
    let count = 0;
    matrix.forEach((row) => {
      row.forEach(() => {
        count = count + 1;
      });
    });
    expect(count).toBe(6);
  });

  test("nested callbacks with array creation", () => {
    const result = [1, 2, 3].map((x) =>
      [4, 5, 6].filter((y) => (x + y) % 2 === 0)
    );
    expect(result).toEqual([[5], [4, 6], [5]]);
  });

  test("deeply nested with mixed methods", () => {
    const data = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    const result = data
      .filter((row) => row.some((x) => x > 3))
      .map((row) => row.filter((x) => x % 2 === 0))
      .reduce((acc, evens) => [...acc, ...evens], []);
    expect(result).toEqual([4, 6, 8]);
  });

  test("map producing arrays then reduce to flatten", () => {
    const words = ["hello", "world"];
    const chars = words
      .map((w) => Array.from(w))
      .reduce((acc, arr) => [...acc, ...arr], []);
    expect(chars).toEqual(["h", "e", "l", "l", "o", "w", "o", "r", "l", "d"]);
  });
});
