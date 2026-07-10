/*---
description: TypedArray operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("TypedArray creation", () => {
  bench("new Int32Array(0)", () => {
    const ta = new Int32Array(0);
  });

  bench("new Int32Array(100)", () => {
    const ta = new Int32Array(100);
  });

  bench("new Int32Array(1000)", () => {
    const ta = new Int32Array(1000);
  });

  bench("new Float64Array(100)", () => {
    const ta = new Float64Array(100);
  });

  bench("Int32Array.from([...])", ({ *setup() {
    const arr = (() => {
      const arr = [];
      let i = 0;
      for (const _ of Array(100).keys()) {
        arr.push(i);
        i = i + 1;
      }
      return arr;
    })();
    yield () => {
      const ta = Int32Array.from(arr);
    };
  } }).setup);

  bench("Int32Array.of(1, 2, 3, 4, 5)", () => {
    const ta = Int32Array.of(1, 2, 3, 4, 5);
  });
});

group("TypedArray element access", () => {
  bench("sequential write 100 elements", ({ *setup() {
    const ta = (() => new Int32Array(100))();
    yield () => {
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i + 1;
      }
    };
  } }).setup);

  bench("sequential read 100 elements", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(42);
      return ta;
    })();
    yield () => {
      let sum = 0;
      for (const idx of Array(100).keys()) {
        sum = sum + ta[idx];
      }
    };
  } }).setup);

  bench("Float64Array write 100 elements", ({ *setup() {
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

group("TypedArray methods", () => {
  bench("fill(42)", ({ *setup() {
    const ta = (() => new Int32Array(1000))();
    yield () => {
      ta.fill(42);
    };
  } }).setup);

  bench("slice()", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    })();
    yield () => {
      const sliced = ta.slice();
    };
  } }).setup);

  bench("map(x => x * 2)", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(5);
      return ta;
    })();
    yield () => {
      const mapped = ta.map((x) => x * 2);
    };
  } }).setup);

  bench("filter(x => x > 50)", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i + 1;
      }
      return ta;
    })();
    yield () => {
      const filtered = ta.filter((x) => x > 50);
    };
  } }).setup);

  bench("reduce (sum)", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    })();
    yield () => {
      const sum = ta.reduce((acc, x) => acc + x, 0);
    };
  } }).setup);

  bench("sort()", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      let i = 100;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i - 1;
      }
      return ta;
    })();
    yield () => {
      ta.sort();
    };
  } }).setup);

  bench("indexOf()", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      let i = 0;
      for (const idx of Array(100).keys()) {
        ta[idx] = i;
        i = i + 1;
      }
      return ta;
    })();
    yield () => {
      const idx = ta.indexOf(50);
    };
  } }).setup);

  bench("reverse()", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    })();
    yield () => {
      ta.reverse();
    };
  } }).setup);
});

group("TypedArray buffer sharing", () => {
  bench("create view over existing buffer", ({ *setup() {
    const buf = (() => new ArrayBuffer(400))();
    yield () => {
      const view = new Int32Array(buf);
    };
  } }).setup);

  bench("subarray()", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(42);
      return ta;
    })();
    yield () => {
      const sub = ta.subarray(10, 50);
    };
  } }).setup);

  bench("set() from array", ({ *setup() {
    const ctx = (() => {
      const ta = new Int32Array(100);
      const src = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      return { ta, src };
    })();
    yield () => {
      ctx.ta.set(ctx.src);
    };
  } }).setup);
});

group("TypedArray iteration", () => {
  bench("for-of loop", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(100);
      ta.fill(1);
      return ta;
    })();
    yield () => {
      let sum = 0;
      for (const v of ta) {
        sum = sum + v;
      }
    };
  } }).setup);

  bench("spread into array", ({ *setup() {
    const ta = (() => {
      const ta = new Int32Array(50);
      ta.fill(1);
      return ta;
    })();
    yield () => {
      const arr = [...ta];
    };
  } }).setup);
});
