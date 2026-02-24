/*---
description: ArrayBuffer operation benchmarks
---*/

suite("ArrayBuffer creation", () => {
  bench("create ArrayBuffer(0)", {
    run: () => {
      const buf = new ArrayBuffer(0);
    },
  });

  bench("create ArrayBuffer(64)", {
    run: () => {
      const buf = new ArrayBuffer(64);
    },
  });

  bench("create ArrayBuffer(1024)", {
    run: () => {
      const buf = new ArrayBuffer(1024);
    },
  });

  bench("create ArrayBuffer(8192)", {
    run: () => {
      const buf = new ArrayBuffer(8192);
    },
  });
});

suite("ArrayBuffer.prototype.slice", () => {
  bench("slice full buffer (64 bytes)", {
    setup: () => new ArrayBuffer(64),
    run: (buf) => {
      const sliced = buf.slice(0);
    },
  });

  bench("slice half buffer (512 of 1024 bytes)", {
    setup: () => new ArrayBuffer(1024),
    run: (buf) => {
      const sliced = buf.slice(0, 512);
    },
  });

  bench("slice with negative indices", {
    setup: () => new ArrayBuffer(256),
    run: (buf) => {
      const sliced = buf.slice(-128, -32);
    },
  });

  bench("slice empty range", {
    setup: () => new ArrayBuffer(1024),
    run: (buf) => {
      const sliced = buf.slice(512, 512);
    },
  });
});

suite("ArrayBuffer property access", () => {
  bench("byteLength access", {
    setup: () => new ArrayBuffer(1024),
    run: (buf) => {
      const len = buf.byteLength;
    },
  });

  bench("Symbol.toStringTag access", {
    setup: () => new ArrayBuffer(64),
    run: (buf) => {
      const tag = buf[Symbol.toStringTag];
    },
  });

  bench("ArrayBuffer.isView", {
    setup: () => new ArrayBuffer(64),
    run: (buf) => {
      const result = ArrayBuffer.isView(buf);
    },
  });
});

suite("structuredClone ArrayBuffer", () => {
  bench("clone ArrayBuffer(64)", {
    setup: () => new ArrayBuffer(64),
    run: (buf) => {
      const clone = structuredClone(buf);
    },
  });

  bench("clone ArrayBuffer(1024)", {
    setup: () => new ArrayBuffer(1024),
    run: (buf) => {
      const clone = structuredClone(buf);
    },
  });

  bench("clone ArrayBuffer inside object", {
    setup: () => ({ buffer: new ArrayBuffer(256), name: "test" }),
    run: (obj) => {
      const clone = structuredClone(obj);
    },
  });
});
