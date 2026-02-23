/*---
description: Destructuring benchmarks
---*/

suite("array destructuring", () => {
  bench("simple array destructuring", () => {
    const arr = [1, 2, 3, 4, 5];
    const [a, b, c, d, e] = arr;
  });

  bench("with rest element", () => {
    const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    const [first, second, ...rest] = arr;
  });

  bench("with defaults", () => {
    const arr = [1, 2];
    const [a, b, c = 3, d = 4, e = 5] = arr;
  });

  bench("skip elements", () => {
    const arr = [1, 2, 3, 4, 5];
    const [a, , c, , e] = arr;
  });

  bench("nested array destructuring", () => {
    const arr = [[1, 2], [3, 4], [5, 6]];
    const [[a, b], [c, d], [e, f]] = arr;
  });

  bench("swap variables", () => {
    let a = 1;
    let b = 2;
    [a, b] = [b, a];
  });
});

suite("object destructuring", () => {
  bench("simple object destructuring", () => {
    const obj = { x: 1, y: 2, z: 3, w: 4 };
    const { x, y, z, w } = obj;
  });

  bench("with defaults", () => {
    const obj = { x: 1 };
    const { x, y = 2, z = 3, w = 4 } = obj;
  });

  bench("with renaming", () => {
    const obj = { firstName: "Alice", lastName: "Smith", age: 30 };
    const { firstName: first, lastName: last, age: years } = obj;
  });

  bench("nested object destructuring", () => {
    const obj = {
      user: { name: "Alice", address: { city: "NYC" } },
      meta: { version: 1 }
    };
    const { user: { name, address: { city } }, meta: { version } } = obj;
  });

  bench("rest properties", () => {
    const obj = { a: 1, b: 2, c: 3, d: 4, e: 5 };
    const { a, b, ...rest } = obj;
  });
});

suite("parameter destructuring", () => {
  bench("object parameter", () => {
    const process = ({ name, value, active = true }) => name + value;
    const r1 = process({ name: "test", value: 42 });
    const r2 = process({ name: "prod", value: 99, active: false });
  });

  bench("array parameter", () => {
    const sum = ([a, b, c]) => a + b + c;
    const r1 = sum([1, 2, 3]);
    const r2 = sum([4, 5, 6]);
  });

  bench("mixed destructuring in map", () => {
    const data = Array.from({ length: 20 }, (_, i) => ({ id: i, value: i * 10 }));
    const values = data.map(({ id, value }) => id + value);
  });
});

suite("callback destructuring", () => {
  bench("forEach with array destructuring", () => {
    const pairs = [[1, 10], [2, 20], [3, 30], [4, 40], [5, 50]];
    let sum = 0;
    pairs.forEach(([k, v]) => { sum = sum + k + v; });
  });

  bench("map with array destructuring", () => {
    const pairs = [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]];
    const sums = pairs.map(([a, b]) => a + b);
  });

  bench("filter with array destructuring", () => {
    const entries = [[1, 10], [2, 5], [3, 20], [4, 3], [5, 15]];
    const big = entries.filter(([, val]) => val > 8);
  });

  bench("reduce with array destructuring", () => {
    const entries = [[1, 10], [2, 20], [3, 30], [4, 40], [5, 50]];
    const total = entries.reduce((acc, [, v]) => acc + v, 0);
  });

  bench("map with object destructuring", () => {
    const items = Array.from({ length: 10 }, (_, i) => ({ id: i, name: "item", value: i * 10 }));
    const ids = items.map(({ id }) => id);
  });

  bench("map with nested destructuring", () => {
    const data = Array.from({ length: 10 }, (_, i) => ({ user: { name: "u" + i }, score: i }));
    const names = data.map(({ user: { name } }) => name);
  });

  bench("map with rest in destructuring", () => {
    const rows = Array.from({ length: 10 }, (_, i) => [i, i + 1, i + 2, i + 3]);
    const firsts = rows.map(([first, ...rest]) => first + rest.length);
  });

  bench("map with defaults in destructuring", () => {
    const items = Array.from({ length: 10 }, (_, i) => ({ x: i }));
    const ys = items.map(({ x, y = 0 }) => x + y);
  });
});
