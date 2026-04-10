/*---
description: Iterator protocol and user-defined iterable benchmarks
---*/

suite("user-defined iterator consumption", () => {
  bench("Iterator.from({next}).toArray() — 20 elements", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 20) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).toArray();
    },
  });

  bench("Iterator.from({next}).toArray() — 50 elements", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).toArray();
    },
  });

  bench("spread pre-wrapped iterator — 20 elements", {
    run: () => {
      let i = 0;
      const iter = Iterator.from({
        next() {
          if (i < 20) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      });
      const arr = [...iter];
    },
  });

  bench("Iterator.from({next}).forEach — 50 elements", {
    run: () => {
      let i = 0;
      let sum = 0;
      Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).forEach((v) => { sum = sum + v; });
    },
  });

  bench("Iterator.from({next}).reduce — 50 elements", {
    run: () => {
      let i = 0;
      const sum = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).reduce((acc, v) => acc + v, 0);
    },
  });
});

suite("Iterator.from", () => {
  bench("wrap array iterator", {
    run: () => {
      const iter = Iterator.from([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
      const arr = iter.toArray();
    },
  });

  bench("wrap plain {next()} object", {
    run: () => {
      let i = 0;
      const iter = Iterator.from({
        next() {
          if (i < 30) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      });
      const arr = iter.toArray();
    },
  });
});

suite("iterator helpers — user-defined source", () => {
  bench("map + toArray (50 elements)", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).map((x) => x * 2).toArray();
    },
  });

  bench("filter + toArray (50 elements)", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).filter((x) => x % 2 === 0).toArray();
    },
  });

  bench("take(10) + toArray (50 element source)", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).take(10).toArray();
    },
  });

  bench("drop(40) + toArray (50 element source)", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).drop(40).toArray();
    },
  });

  bench("chained map + filter + take (100 element source)", {
    run: () => {
      let i = 0;
      const arr = Iterator.from({
        next() {
          if (i < 100) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).map((x) => x * 3)
        .filter((x) => x % 2 === 0)
        .take(10)
        .toArray();
    },
  });

  bench("some + every (50 elements)", {
    run: () => {
      let i = 0;
      const a = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).some((x) => x > 40);
      let j = 0;
      const b = Iterator.from({
        next() {
          if (j < 50) { j = j + 1; return { value: j, done: false }; }
          return { value: undefined, done: true };
        }
      }).every((x) => x < 100);
    },
  });

  bench("find (50 elements)", {
    run: () => {
      let i = 0;
      const found = Iterator.from({
        next() {
          if (i < 50) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      }).find((x) => x === 42);
    },
  });
});

suite("Iterator.concat", () => {
  const arr10 = Array.from({ length: 10 }, (_, i) => i);
  const arr20 = Array.from({ length: 20 }, (_, i) => i);

  bench("concat 2 arrays (10 + 10 elements)", {
    run: () => {
      const result = Iterator.concat(arr10, arr10).toArray();
    },
  });

  bench("concat 5 arrays (10 elements each)", {
    run: () => {
      const result = Iterator.concat(arr10, arr10, arr10, arr10, arr10).toArray();
    },
  });

  bench("concat 2 arrays (20 + 20 elements)", {
    run: () => {
      const result = Iterator.concat(arr20, arr20).toArray();
    },
  });

  bench("concat + filter + toArray (20 + 20 elements)", {
    run: () => {
      const result = Iterator.concat(arr20, arr20)
        .filter((x) => x % 2 === 0)
        .toArray();
    },
  });

  bench("concat + map + take (20 + 20 elements, take 10)", {
    run: () => {
      const result = Iterator.concat(arr20, arr20)
        .map((x) => x * 3)
        .take(10)
        .toArray();
    },
  });

  bench("concat Sets (15 + 15 elements)", {
    setup: () => ({
      a: new Set(Array.from({ length: 15 }, (_, i) => i)),
      b: new Set(Array.from({ length: 15 }, (_, i) => i + 15)),
    }),
    run: (ctx) => {
      const result = Iterator.concat(ctx.a, ctx.b).toArray();
    },
  });

  bench("concat strings (13 + 13 characters)", {
    run: () => {
      const result = Iterator.concat("abcdefghijklm", "nopqrstuvwxyz").toArray();
    },
  });
});

suite("Iterator.zip", () => {
  const arr10 = Array.from({ length: 10 }, (_, i) => i);
  const arr20 = Array.from({ length: 20 }, (_, i) => i);
  const arr50 = Array.from({ length: 50 }, (_, i) => i);

  bench("zip 2 arrays (10 + 10 elements)", {
    run: () => {
      const result = Iterator.zip([arr10, arr10]).toArray();
    },
  });

  bench("zip 3 arrays (10 elements each)", {
    run: () => {
      const result = Iterator.zip([arr10, arr10, arr10]).toArray();
    },
  });

  bench("zip 2 arrays (20 + 20 elements)", {
    run: () => {
      const result = Iterator.zip([arr20, arr20]).toArray();
    },
  });

  bench("zip 2 arrays (50 + 50 elements)", {
    run: () => {
      const result = Iterator.zip([arr50, arr50]).toArray();
    },
  });

  bench("zip shortest mode (20 + 10 elements)", {
    run: () => {
      const result = Iterator.zip([arr20, arr10]).toArray();
    },
  });

  bench("zip longest mode (10 + 20 elements)", {
    run: () => {
      const result = Iterator.zip([arr10, arr20], { mode: "longest", padding: [0, 0] }).toArray();
    },
  });

  bench("zip strict mode (20 + 20 elements)", {
    run: () => {
      const result = Iterator.zip([arr20, arr20], { mode: "strict" }).toArray();
    },
  });

  bench("zip + map + toArray (20 + 20 elements)", {
    run: () => {
      const result = Iterator.zip([arr20, arr20])
        .map(([a, b]) => a + b)
        .toArray();
    },
  });

  bench("zip + filter + toArray (20 + 20 elements)", {
    run: () => {
      const result = Iterator.zip([arr20, arr20])
        .filter(([a, b]) => a % 2 === 0)
        .toArray();
    },
  });

  bench("zip Sets (15 + 15 elements)", {
    setup: () => ({
      a: new Set(Array.from({ length: 15 }, (_, i) => i)),
      b: new Set(Array.from({ length: 15 }, (_, i) => i + 15)),
    }),
    run: (ctx) => {
      const result = Iterator.zip([ctx.a, ctx.b]).toArray();
    },
  });
});

suite("Iterator.zipKeyed", () => {
  bench("zipKeyed 2 keys (10 elements each)", {
    setup: () => ({
      x: Array.from({ length: 10 }, (_, i) => i),
      y: Array.from({ length: 10 }, (_, i) => i * 2),
    }),
    run: (data) => {
      const result = Iterator.zipKeyed(data).toArray();
    },
  });

  bench("zipKeyed 3 keys (20 elements each)", {
    setup: () => ({
      name: Array.from({ length: 20 }, (_, i) => "item" + i),
      value: Array.from({ length: 20 }, (_, i) => i * 10),
      flag: Array.from({ length: 20 }, (_, i) => i % 2 === 0),
    }),
    run: (data) => {
      const result = Iterator.zipKeyed(data).toArray();
    },
  });

  bench("zipKeyed longest mode (10 + 20 elements)", {
    setup: () => ({
      short: Array.from({ length: 10 }, (_, i) => i),
      long: Array.from({ length: 20 }, (_, i) => i),
    }),
    run: (data) => {
      const result = Iterator.zipKeyed(data, {
        mode: "longest",
        padding: { short: 0, long: 0 },
      }).toArray();
    },
  });

  bench("zipKeyed strict mode (20 + 20 elements)", {
    setup: () => ({
      a: Array.from({ length: 20 }, (_, i) => i),
      b: Array.from({ length: 20 }, (_, i) => i * 3),
    }),
    run: (data) => {
      const result = Iterator.zipKeyed(data, { mode: "strict" }).toArray();
    },
  });

  bench("zipKeyed + filter + map (20 elements)", {
    setup: () => ({
      x: Array.from({ length: 20 }, (_, i) => i),
      y: Array.from({ length: 20 }, (_, i) => i * 5),
    }),
    run: (data) => {
      const result = Iterator.zipKeyed(data)
        .filter(({ x }) => x > 10)
        .map(({ x, y }) => x + y)
        .toArray();
    },
  });
});

suite("built-in iterator helpers", () => {
  const arr50 = Array.from({ length: 50 }, (_, i) => i);

  bench("array.values().map().filter().toArray()", {
    run: () => {
      const result = arr50.values()
        .map((x) => x * 2)
        .filter((x) => x > 20)
        .toArray();
    },
  });

  bench("array.values().take(5).toArray()", {
    run: () => {
      const result = arr50.values().take(5).toArray();
    },
  });

  bench("array.values().drop(45).toArray()", {
    run: () => {
      const result = arr50.values().drop(45).toArray();
    },
  });

  bench("map.entries() chained helpers", {
    setup: () => new Map(Array.from({ length: 30 }, (_, i) => ["k" + i, i])),
    run: (m) => {
      const result = m.entries()
        .filter(([k, v]) => v > 10)
        .map(([k, v]) => v * 2)
        .toArray();
    },
  });

  bench("set.values() chained helpers", {
    setup: () => new Set(Array.from({ length: 30 }, (_, i) => i)),
    run: (s) => {
      const result = s.values()
        .filter((x) => x % 3 === 0)
        .map((x) => x * 10)
        .toArray();
    },
  });

  bench("string iterator map + toArray", {
    run: () => {
      const result = "abcdefghijklmnopqrstuvwxyz"[Symbol.iterator]()
        .map((ch) => ch.toUpperCase())
        .toArray();
    },
  });
});
