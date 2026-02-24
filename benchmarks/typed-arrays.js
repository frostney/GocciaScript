/*---
description: TypedArray operation benchmarks
---*/

suite("TypedArray creation", () => {
  bench("new Int32Array(0)", {
    run: () => {
      const ta = new Int32Array(0);
    },
  });

  bench("new Int32Array(100)", {
    run: () => {
      const ta = new Int32Array(100);
    },
  });

  bench("new Int32Array(1000)", {
    run: () => {
      const ta = new Int32Array(1000);
    },
  });

  bench("new Float64Array(100)", {
    run: () => {
      const ta = new Float64Array(100);
    },
  });

  bench("Int32Array.from([...])", {
    setup: () => {
      const arr = [];
      let i = 0;
      for (const _ of Array(100).keys()) {
        arr.push(i);
        i = i + 1;
      }
      return arr;
    },
    run: (arr) => {
      const ta = Int32Array.from(arr);
    },
  });

  bench("Int32Array.of(1, 2, 3, 4, 5)", {
    run: () => {
      const ta = Int32Array.of(1, 2, 3, 4, 5);
    },
  });
});

suite("TypedArray element access", () => {
  bench("sequential write 100 elements", {
    setup: () => new Int32Array(100),
    run: (ta) => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i + 1;
      }
    },
  });

  bench("sequential read 100 elements", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(42);
      return ta;
    },
    run: (ta) => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + ta[idx];
      }
    },
  });

  bench("Float64Array write 100 elements", {
    setup: () => new Float64Array(100),
    run: (ta) => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 1.5;
        i = i + 1;
      }
    },
  });
});

suite("TypedArray methods", () => {
  bench("fill(42)", {
    setup: () => new Int32Array(1000),
    run: (ta) => {
      ta.fill(42);
    },
  });

  bench("slice()", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    },
    run: (ta) => {
      const sliced = ta.slice();
    },
  });

  bench("map(x => x * 2)", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(5);
      return ta;
    },
    run: (ta) => {
      const mapped = ta.map((x) => x * 2);
    },
  });

  bench("filter(x => x > 50)", {
    setup: () => {
      const ta = new Int32Array(100);
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i + 1;
      }
      return ta;
    },
    run: (ta) => {
      const filtered = ta.filter((x) => x > 50);
    },
  });

  bench("reduce (sum)", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    },
    run: (ta) => {
      const sum = ta.reduce((acc, x) => acc + x, 0);
    },
  });

  bench("sort()", {
    setup: () => {
      const ta = new Int32Array(100);
      let i = 100;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i - 1;
      }
      return ta;
    },
    run: (ta) => {
      ta.sort();
    },
  });

  bench("indexOf()", {
    setup: () => {
      const ta = new Int32Array(100);
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i + 1;
      }
      return ta;
    },
    run: (ta) => {
      const idx = ta.indexOf(50);
    },
  });

  bench("reverse()", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    },
    run: (ta) => {
      ta.reverse();
    },
  });
});

suite("TypedArray buffer sharing", () => {
  bench("create view over existing buffer", {
    setup: () => new ArrayBuffer(400),
    run: (buf) => {
      const view = new Int32Array(buf);
    },
  });

  bench("subarray()", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(42);
      return ta;
    },
    run: (ta) => {
      const sub = ta.subarray(10, 50);
    },
  });

  bench("set() from array", {
    setup: () => {
      const ta = new Int32Array(100);
      const src = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      return { ta, src };
    },
    run: (ctx) => {
      ctx.ta.set(ctx.src);
    },
  });
});

suite("TypedArray iteration", () => {
  bench("for-of loop", {
    setup: () => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    },
    run: (ta) => {
      let sum = 0;
      for (const v of ta) {
        sum = sum + v;
      }
    },
  });

  bench("spread into array", {
    setup: () => {
      const ta = new Int32Array(50);
      ta.fill(1);
      return ta;
    },
    run: (ta) => {
      const arr = [...ta];
    },
  });
});

runBenchmarks();
