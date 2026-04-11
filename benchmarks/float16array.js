/*---
description: Float16Array operation benchmarks — IEEE 754 half-precision (binary16)
---*/

// Pre-populate test data
const F16_100 = new Float16Array(100);
const F16_1000 = new Float16Array(1000);
const F32_100 = new Float32Array(100);
const F64_100 = new Float64Array(100);

for (const i of Array(100).keys()) {
  F16_100[i] = i * 0.5;
  F32_100[i] = i * 0.5;
  F64_100[i] = i * 0.5;
}
for (const i of Array(1000).keys()) F16_1000[i] = (i % 500) * 0.1;

suite("Float16Array creation", () => {
  bench("new Float16Array(0)", {
    run: () => {
      const ta = new Float16Array(0);
    },
  });

  bench("new Float16Array(100)", {
    run: () => {
      const ta = new Float16Array(100);
    },
  });

  bench("new Float16Array(1000)", {
    run: () => {
      const ta = new Float16Array(1000);
    },
  });

  bench("Float16Array.from([...100])", {
    setup: () => {
      const arr = [];
      let i = 0;
      for (const _ of Array(100).keys()) {
        arr.push(i * 0.5);
        i = i + 1;
      }
      return arr;
    },
    run: (arr) => {
      const ta = Float16Array.from(arr);
    },
  });

  bench("Float16Array.of(1.5, 2.5, 3.5, 4.5, 5.5)", {
    run: () => {
      const ta = Float16Array.of(1.5, 2.5, 3.5, 4.5, 5.5);
    },
  });

  bench("new Float16Array(float64Array)", {
    setup: () => {
      const f64 = new Float64Array(100);
      for (const i of Array(100).keys()) f64[i] = i * 0.5;
      return f64;
    },
    run: (f64) => {
      const f16 = new Float16Array(f64);
    },
  });
});

suite("Float16Array element access", () => {
  bench("sequential write 100 elements", {
    setup: () => new Float16Array(100),
    run: (ta) => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 0.5;
        i = i + 1;
      }
    },
  });

  bench("sequential read 100 elements", {
    setup: () => {
      const ta = new Float16Array(100);
      ta.fill(1.5);
      return ta;
    },
    run: (ta) => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + ta[idx];
      }
    },
  });

  bench("write special values (NaN, Inf, -0)", {
    setup: () => new Float16Array(6),
    run: (ta) => {
      ta[0] = NaN;
      ta[1] = Infinity;
      ta[2] = -Infinity;
      ta[3] = -0;
      ta[4] = 65504;
      ta[5] = 5.960464477539063e-8;
    },
  });
});

suite("Float16Array vs Float32Array vs Float64Array — write 100", () => {
  bench("Float16Array write", {
    setup: () => new Float16Array(100),
    run: (ta) => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 1.5;
        i = i + 1;
      }
    },
  });

  bench("Float32Array write", {
    setup: () => new Float32Array(100),
    run: (ta) => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 1.5;
        i = i + 1;
      }
    },
  });

  bench("Float64Array write", {
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

suite("Float16Array vs Float32Array vs Float64Array — read 100", () => {
  bench("Float16Array read", {
    run: () => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + F16_100[idx];
      }
    },
  });

  bench("Float32Array read", {
    run: () => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + F32_100[idx];
      }
    },
  });

  bench("Float64Array read", {
    run: () => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + F64_100[idx];
      }
    },
  });
});

suite("Float16Array methods", () => {
  bench("fill(1.5)", {
    setup: () => new Float16Array(1000),
    run: (ta) => {
      ta.fill(1.5);
    },
  });

  bench("slice()", {
    setup: () => {
      const ta = new Float16Array(100);
      ta.fill(1.5);
      return ta;
    },
    run: (ta) => {
      const sliced = ta.slice();
    },
  });

  bench("map(x => x * 2)", {
    run: () => {
      const mapped = F16_100.map((x) => x * 2);
    },
  });

  bench("filter(x => x > 25)", {
    run: () => {
      const filtered = F16_100.filter((x) => x > 25);
    },
  });

  bench("reduce (sum)", {
    run: () => {
      const sum = F16_100.reduce((acc, x) => acc + x, 0);
    },
  });

  bench("sort()", {
    setup: () => {
      const ta = new Float16Array(100);
      let i = 100;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 0.5;
        i = i - 1;
      }
      return ta;
    },
    run: (ta) => {
      ta.sort();
    },
  });

  bench("indexOf()", {
    run: () => {
      const idx = F16_100.indexOf(25);
    },
  });

  bench("reverse()", {
    setup: () => {
      const ta = new Float16Array(100);
      ta.fill(1.5);
      return ta;
    },
    run: (ta) => {
      ta.reverse();
    },
  });

  bench("toReversed()", {
    run: () => {
      const rev = F16_100.toReversed();
    },
  });

  bench("toSorted()", {
    setup: () => {
      const ta = new Float16Array(100);
      let i = 100;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 0.5;
        i = i - 1;
      }
      return ta;
    },
    run: (ta) => {
      const sorted = ta.toSorted();
    },
  });
});

suite("Float16Array buffer sharing", () => {
  bench("create view over existing buffer", {
    setup: () => new ArrayBuffer(200),
    run: (buf) => {
      const view = new Float16Array(buf);
    },
  });

  bench("subarray()", {
    run: () => {
      const sub = F16_100.subarray(10, 50);
    },
  });

  bench("set() from array", {
    setup: () => {
      const ta = new Float16Array(100);
      const src = [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5];
      return { ta, src };
    },
    run: (ctx) => {
      ctx.ta.set(ctx.src);
    },
  });
});

suite("Float16Array iteration", () => {
  bench("for-of loop", {
    run: () => {
      let sum = 0;
      for (const v of F16_100) {
        sum = sum + v;
      }
    },
  });

  bench("spread into array", {
    setup: () => {
      const ta = new Float16Array(50);
      ta.fill(1.5);
      return ta;
    },
    run: (ta) => {
      const arr = [...ta];
    },
  });
});

suite("Math.f16round", () => {
  bench("f16round(1.337)", {
    run: () => {
      const r = Math.f16round(1.337);
    },
  });

  bench("f16round over 100 values", {
    setup: () => {
      const values = [];
      for (const i of Array(100).keys()) values.push(i * 1.337);
      return values;
    },
    run: (values) => {
      for (const v of values) {
        const r = Math.f16round(v);
      }
    },
  });
});

runBenchmarks();
