/*---
description: Atomics and SharedArrayBuffer operation benchmarks
---*/

import { bench, group } from "goccia:microbench";

group("Atomics integer operations", () => {
  bench("load and store Int32Array", ({ *setup() {
    const view = (() => new Int32Array(new SharedArrayBuffer(4)))();
    yield () => {
      Atomics.store(view, 0, 41);
      const value = Atomics.load(view, 0);
    };
  } }).setup);

  bench("read-modify-write Int32Array", ({ *setup() {
    const view = (() => new Int32Array(new SharedArrayBuffer(4)))();
    yield () => {
      Atomics.add(view, 0, 1);
      Atomics.sub(view, 0, 1);
      Atomics.exchange(view, 0, 7);
    };
  } }).setup);

  bench("compareExchange hit and miss", ({ *setup() {
    const view = (() => {
      const view = new Int32Array(new SharedArrayBuffer(4));
      view[0] = 1;
      return view;
    })();
    yield () => {
      Atomics.compareExchange(view, 0, 1, 2);
      Atomics.compareExchange(view, 0, 1, 3);
      Atomics.compareExchange(view, 0, 2, 1);
    };
  } }).setup);
});

group("Atomics wait and notify", () => {
  bench("wait with zero timeout", ({ *setup() {
    const view = (() => {
      const view = new Int32Array(new SharedArrayBuffer(4));
      view[0] = 1;
      return view;
    })();
    yield () => {
      const result = Atomics.wait(view, 0, 1, 0);
    };
  } }).setup);

  bench("waitAsync synchronous not-equal", ({ *setup() {
    const view = (() => {
      const view = new Int32Array(new SharedArrayBuffer(4));
      view[0] = 1;
      return view;
    })();
    yield () => {
      const result = Atomics.waitAsync(view, 0, 2, 10);
    };
  } }).setup);

  bench("notify with no waiters", ({ *setup() {
    const view = (() => new Int32Array(new SharedArrayBuffer(4)))();
    yield () => {
      const count = Atomics.notify(view, 0, 1);
    };
  } }).setup);
});
