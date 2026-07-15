/*---
description: Array operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("array creation", () => {
  bench("Array.from length 100", () => {
    const arr = Array.from({ length: 100 }, (_, i) => i);
  });

  bench("Array.from 10 elements", () => {
    const arr = Array.from([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  });

  bench("Array.of 10 elements", () => {
    const arr = Array.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  });

  bench("spread into new array", () => {
    const source = [1, 2, 3, 4, 5];
    const copy = [...source, ...source];
  });
});

group("array iteration", () => {
  const arr50 = Array.from({ length: 50 }, (_, i) => i);

  bench("map over 50 elements", () => {
    const doubled = arr50.map((x) => x * 2);
  });

  bench("filter over 50 elements", () => {
    const evens = arr50.filter((x) => x % 2 === 0);
  });

  bench("reduce sum 50 elements", () => {
    const sum = arr50.reduce((acc, x) => acc + x, 0);
  });

  bench("forEach over 50 elements", () => {
    let count = 0;
    arr50.forEach((x) => { count = count + 1; });
  });

  bench("find in 50 elements", () => {
    const found = arr50.find((x) => x === 42);
  });
});

group("array transformation", () => {
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

group("nested callbacks", () => {
  bench("map inside map (5x5)", ({ *setup() {
    const matrix = (() => [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15], [16, 17, 18, 19, 20], [21, 22, 23, 24, 25]])();
    yield () => {
      const result = matrix.map((row) => row.map((x) => x * 2));
    };
  } }).setup);

  bench("filter inside map (5x10)", ({ *setup() {
    const data = (() => Array.from({ length: 5 }, (_, i) => Array.from({ length: 10 }, (_, j) => i * 10 + j)))();
    yield () => {
      const result = data.map((row) => row.filter((x) => x % 2 === 0));
    };
  } }).setup);

  bench("reduce inside map (5x10)", ({ *setup() {
    const data = (() => Array.from({ length: 5 }, (_, i) => Array.from({ length: 10 }, (_, j) => i * 10 + j)))();
    yield () => {
      const sums = data.map((row) => row.reduce((a, b) => a + b, 0));
    };
  } }).setup);

  bench("forEach inside forEach (5x10)", ({ *setup() {
    const matrix = (() => Array.from({ length: 5 }, (_, i) => Array.from({ length: 10 }, (_, j) => i * 10 + j)))();
    yield () => {
      let sum = 0;
      matrix.forEach((row) => { row.forEach((x) => { sum = sum + x; }); });
    };
  } }).setup);

  bench("find inside some (10x10)", ({ *setup() {
    const groups = (() => Array.from({ length: 10 }, (_, i) => Array.from({ length: 10 }, (_, j) => i * 10 + j)))();
    yield () => {
      const found = groups.some((g) => g.find((x) => x === 77) !== undefined);
    };
  } }).setup);

  bench("map+filter chain nested (5x20)", ({ *setup() {
    const data = (() => Array.from({ length: 5 }, (_, i) => Array.from({ length: 20 }, (_, j) => i * 20 + j)))();
    yield () => {
      const result = data.map((row) => row.map((x) => x * 3).filter((x) => x % 2 === 0));
    };
  } }).setup);

  bench("reduce flatten (10x5)", ({ *setup() {
    const matrix = (() => Array.from({ length: 10 }, (_, i) => Array.from({ length: 5 }, (_, j) => i * 5 + j)))();
    yield () => {
      const flat = matrix.reduce((acc, row) => [...acc, ...row], []);
    };
  } }).setup);
});
