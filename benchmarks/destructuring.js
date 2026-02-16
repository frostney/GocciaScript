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
