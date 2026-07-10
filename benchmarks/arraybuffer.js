/*---
description: ArrayBuffer operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("ArrayBuffer creation", () => {
  bench("create ArrayBuffer(0)", () => {
    const buf = new ArrayBuffer(0);
  });

  bench("create ArrayBuffer(64)", () => {
    const buf = new ArrayBuffer(64);
  });

  bench("create ArrayBuffer(1024)", () => {
    const buf = new ArrayBuffer(1024);
  });

  bench("create ArrayBuffer(8192)", () => {
    const buf = new ArrayBuffer(8192);
  });
});

group("ArrayBuffer.prototype.slice", () => {
  bench("slice full buffer (64 bytes)", ({ *setup() {
    const buf = (() => new ArrayBuffer(64))();
    yield () => {
      const sliced = buf.slice(0);
    };
  } }).setup);

  bench("slice half buffer (512 of 1024 bytes)", ({ *setup() {
    const buf = (() => new ArrayBuffer(1024))();
    yield () => {
      const sliced = buf.slice(0, 512);
    };
  } }).setup);

  bench("slice with negative indices", ({ *setup() {
    const buf = (() => new ArrayBuffer(256))();
    yield () => {
      const sliced = buf.slice(-128, -32);
    };
  } }).setup);

  bench("slice empty range", ({ *setup() {
    const buf = (() => new ArrayBuffer(1024))();
    yield () => {
      const sliced = buf.slice(512, 512);
    };
  } }).setup);
});

group("ArrayBuffer property access", () => {
  bench("byteLength access", ({ *setup() {
    const buf = (() => new ArrayBuffer(1024))();
    yield () => {
      const len = buf.byteLength;
    };
  } }).setup);

  bench("Symbol.toStringTag access", ({ *setup() {
    const buf = (() => new ArrayBuffer(64))();
    yield () => {
      const tag = buf[Symbol.toStringTag];
    };
  } }).setup);

  bench("ArrayBuffer.isView", ({ *setup() {
    const buf = (() => new ArrayBuffer(64))();
    yield () => {
      const result = ArrayBuffer.isView(buf);
    };
  } }).setup);
});

group("structuredClone ArrayBuffer", () => {
  bench("clone ArrayBuffer(64)", ({ *setup() {
    const buf = (() => new ArrayBuffer(64))();
    yield () => {
      const clone = structuredClone(buf);
    };
  } }).setup);

  bench("clone ArrayBuffer(1024)", ({ *setup() {
    const buf = (() => new ArrayBuffer(1024))();
    yield () => {
      const clone = structuredClone(buf);
    };
  } }).setup);

  bench("clone ArrayBuffer inside object", ({ *setup() {
    const obj = (() => ({ buffer: new ArrayBuffer(256), name: "test" }))();
    yield () => {
      const clone = structuredClone(obj);
    };
  } }).setup);
});
