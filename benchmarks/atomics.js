/*---
description: Atomics and SharedArrayBuffer operation benchmarks
---*/

suite("Atomics integer operations", () => {
  bench("load and store Int32Array", {
    setup: () => new Int32Array(new SharedArrayBuffer(4)),
    run: (view) => {
      Atomics.store(view, 0, 41);
      const value = Atomics.load(view, 0);
    },
  });

  bench("read-modify-write Int32Array", {
    setup: () => new Int32Array(new SharedArrayBuffer(4)),
    run: (view) => {
      Atomics.add(view, 0, 1);
      Atomics.sub(view, 0, 1);
      Atomics.exchange(view, 0, 7);
    },
  });

  bench("compareExchange hit and miss", {
    setup: () => {
      const view = new Int32Array(new SharedArrayBuffer(4));
      view[0] = 1;
      return view;
    },
    run: (view) => {
      Atomics.compareExchange(view, 0, 1, 2);
      Atomics.compareExchange(view, 0, 1, 3);
      Atomics.compareExchange(view, 0, 2, 1);
    },
  });
});

suite("Atomics wait and notify", () => {
  bench("wait with zero timeout", {
    setup: () => {
      const view = new Int32Array(new SharedArrayBuffer(4));
      view[0] = 1;
      return view;
    },
    run: (view) => {
      const result = Atomics.wait(view, 0, 1, 0);
    },
  });

  bench("waitAsync synchronous not-equal", {
    setup: () => {
      const view = new Int32Array(new SharedArrayBuffer(4));
      view[0] = 1;
      return view;
    },
    run: (view) => {
      const result = Atomics.waitAsync(view, 0, 2, 10);
    },
  });

  bench("notify with no waiters", {
    setup: () => new Int32Array(new SharedArrayBuffer(4)),
    run: (view) => {
      const count = Atomics.notify(view, 0, 1);
    },
  });
});
