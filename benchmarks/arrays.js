/*---
description: Array operation benchmarks
---*/

suite("array creation", () => {
  bench("Array.from length 100", () => {
    const arr = Array.from({ length: 100 }, (_, i) => i);
  });

  bench("Array.of 10 elements", () => {
    const arr = Array.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  });

  bench("spread into new array", () => {
    const source = [1, 2, 3, 4, 5];
    const copy = [...source, ...source];
  });
});

suite("array iteration", () => {
  bench("map over 50 elements", () => {
    const arr = Array.from({ length: 50 }, (_, i) => i);
    const doubled = arr.map((x) => x * 2);
  });

  bench("filter over 50 elements", () => {
    const arr = Array.from({ length: 50 }, (_, i) => i);
    const evens = arr.filter((x) => x % 2 === 0);
  });

  bench("reduce sum 50 elements", () => {
    const arr = Array.from({ length: 50 }, (_, i) => i);
    const sum = arr.reduce((acc, x) => acc + x, 0);
  });

  bench("forEach over 50 elements", () => {
    const arr = Array.from({ length: 50 }, (_, i) => i);
    let count = 0;
    arr.forEach((x) => { count = count + 1; });
  });

  bench("find in 50 elements", () => {
    const arr = Array.from({ length: 50 }, (_, i) => i);
    const found = arr.find((x) => x === 42);
  });
});

suite("array transformation", () => {
  bench("sort 20 elements", () => {
    const arr = [15, 3, 8, 1, 19, 7, 12, 4, 17, 2, 11, 6, 14, 9, 20, 5, 16, 10, 13, 18];
    const sorted = arr.toSorted((a, b) => a - b);
  });

  bench("flat nested array", () => {
    const arr = [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]];
    const flat = arr.flat();
  });

  bench("flatMap", () => {
    const arr = [1, 2, 3, 4, 5];
    const result = arr.flatMap((x) => [x, x * 2]);
  });
});
