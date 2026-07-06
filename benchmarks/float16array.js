/*---
description: Float16Array operation benchmarks — IEEE 754 half-precision (binary16)
---*/

import { bench, group } from "goccia:microbench";

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

group("Float16Array creation", () => {
  bench("new Float16Array(0)", () => {
    const ta = new Float16Array(0);
  });

  bench("new Float16Array(100)", () => {
    const ta = new Float16Array(100);
  });

  bench("new Float16Array(1000)", () => {
    const ta = new Float16Array(1000);
  });

  bench("Float16Array.from([...100])", ({ *setup() {
    const arr = (() => {
      const arr = [];
      let i = 0;
      for (const _ of Array(100).keys()) {
        arr.push(i * 0.5);
        i = i + 1;
      }
      return arr;
    })();
    yield () => {
      const ta = Float16Array.from(arr);
    };
  } }).setup);

  bench("Float16Array.of(1.5, 2.5, 3.5, 4.5, 5.5)", () => {
    const ta = Float16Array.of(1.5, 2.5, 3.5, 4.5, 5.5);
  });

  bench("new Float16Array(float64Array)", ({ *setup() {
    const f64 = (() => {
      const f64 = new Float64Array(100);
      for (const i of Array(100).keys()) f64[i] = i * 0.5;
      return f64;
    })();
    yield () => {
      const f16 = new Float16Array(f64);
    };
  } }).setup);
});

group("Float16Array element access", () => {
  bench("sequential write 100 elements", ({ *setup() {
    const ta = (() => new Float16Array(100))();
    yield () => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 0.5;
        i = i + 1;
      }
    };
  } }).setup);

  bench("sequential read 100 elements", ({ *setup() {
    const ta = (() => {
      const ta = new Float16Array(100);
      ta.fill(1.5);
      return ta;
    })();
    yield () => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + ta[idx];
      }
    };
  } }).setup);

  bench("write special values (NaN, Inf, -0)", ({ *setup() {
    const ta = (() => new Float16Array(6))();
    yield () => {
      ta[0] = NaN;
      ta[1] = Infinity;
      ta[2] = -Infinity;
      ta[3] = -0;
      ta[4] = 65504;
      ta[5] = 5.960464477539063e-8;
    };
  } }).setup);
});

group("Float16Array vs Float32Array vs Float64Array — write 100", () => {
  bench("Float16Array write", ({ *setup() {
    const ta = (() => new Float16Array(100))();
    yield () => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 1.5;
        i = i + 1;
      }
    };
  } }).setup);

  bench("Float32Array write", ({ *setup() {
    const ta = (() => new Float32Array(100))();
    yield () => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 1.5;
        i = i + 1;
      }
    };
  } }).setup);

  bench("Float64Array write", ({ *setup() {
    const ta = (() => new Float64Array(100))();
    yield () => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 1.5;
        i = i + 1;
      }
    };
  } }).setup);
});

group("Float16Array vs Float32Array vs Float64Array — read 100", () => {
  bench("Float16Array read", () => {
    let sum = 0;
    for (const idx of Array(100).keys()) {
      sum = sum + F16_100[idx];
    }
  });

  bench("Float32Array read", () => {
    let sum = 0;
    for (const idx of Array(100).keys()) {
      sum = sum + F32_100[idx];
    }
  });

  bench("Float64Array read", () => {
    let sum = 0;
    for (const idx of Array(100).keys()) {
      sum = sum + F64_100[idx];
    }
  });
});

group("Float16Array methods", () => {
  bench("fill(1.5)", ({ *setup() {
    const ta = (() => new Float16Array(1000))();
    yield () => {
      ta.fill(1.5);
    };
  } }).setup);

  bench("slice()", ({ *setup() {
    const ta = (() => {
      const ta = new Float16Array(100);
      ta.fill(1.5);
      return ta;
    })();
    yield () => {
      const sliced = ta.slice();
    };
  } }).setup);

  bench("map(x => x * 2)", () => {
    const mapped = F16_100.map((x) => x * 2);
  });

  bench("filter(x => x > 25)", () => {
    const filtered = F16_100.filter((x) => x > 25);
  });

  bench("reduce (sum)", () => {
    const sum = F16_100.reduce((acc, x) => acc + x, 0);
  });

  bench("sort()", ({ *setup() {
    const ta = (() => {
      const ta = new Float16Array(100);
      let i = 100;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 0.5;
        i = i - 1;
      }
      return ta;
    })();
    yield () => {
      ta.sort();
    };
  } }).setup);

  bench("indexOf()", () => {
    const idx = F16_100.indexOf(25);
  });

  bench("reverse()", ({ *setup() {
    const ta = (() => {
      const ta = new Float16Array(100);
      ta.fill(1.5);
      return ta;
    })();
    yield () => {
      ta.reverse();
    };
  } }).setup);

  bench("toReversed()", () => {
    const rev = F16_100.toReversed();
  });

  bench("toSorted()", ({ *setup() {
    const ta = (() => {
      const ta = new Float16Array(100);
      let i = 100;
      for (const idx of Array(100).keys()) {
        ta[idx] = i * 0.5;
        i = i - 1;
      }
      return ta;
    })();
    yield () => {
      const sorted = ta.toSorted();
    };
  } }).setup);
});

group("Float16Array buffer sharing", () => {
  bench("create view over existing buffer", ({ *setup() {
    const buf = (() => new ArrayBuffer(200))();
    yield () => {
      const view = new Float16Array(buf);
    };
  } }).setup);

  bench("subarray()", () => {
    const sub = F16_100.subarray(10, 50);
  });

  bench("set() from array", ({ *setup() {
    const ctx = (() => {
      const ta = new Float16Array(100);
      const src = [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5];
      return { ta, src };
    })();
    yield () => {
      ctx.ta.set(ctx.src);
    };
  } }).setup);
});

group("Float16Array iteration", () => {
  bench("for-of loop", () => {
    let sum = 0;
    for (const v of F16_100) {
      sum = sum + v;
    }
  });

  bench("spread into array", ({ *setup() {
    const ta = (() => {
      const ta = new Float16Array(50);
      ta.fill(1.5);
      return ta;
    })();
    yield () => {
      const arr = [...ta];
    };
  } }).setup);
});

group("Math.f16round", () => {
  bench("f16round(1.337)", () => {
    const r = Math.f16round(1.337);
  });

  bench("f16round over 100 values", ({ *setup() {
    const values = (() => {
      const values = [];
      for (const i of Array(100).keys()) values.push(i * 1.337);
      return values;
    })();
    yield () => {
      for (const v of values) {
        const r = Math.f16round(v);
      }
    };
  } }).setup);
});
