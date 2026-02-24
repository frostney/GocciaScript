/*---
description: Promise operation benchmarks
---*/

suite("promises", () => {
  bench("Promise.resolve(value)", {
    run: () => {
      Promise.resolve(42);
    },
  });

  bench("new Promise(resolve => resolve(value))", {
    run: () => {
      new Promise((resolve) => resolve(42));
    },
  });

  bench("Promise.reject(reason)", {
    run: () => {
      Promise.reject("err");
    },
  });

  bench("resolve + then (1 handler)", {
    run: () => {
      Promise.resolve(1).then((v) => v + 1);
    },
  });

  bench("resolve + then chain (3 deep)", {
    run: () => {
      Promise.resolve(1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1);
    },
  });

  bench("resolve + then chain (10 deep)", {
    run: () => {
      Promise.resolve(0)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1)
        .then((v) => v + 1);
    },
  });

  bench("reject + catch + then", {
    run: () => {
      Promise.reject("error")
        .catch((e) => "recovered")
        .then((v) => v);
    },
  });

  bench("resolve + finally + then", {
    run: () => {
      Promise.resolve(42)
        .finally(() => {})
        .then((v) => v);
    },
  });

  bench("Promise.all (5 resolved)", {
    run: () => {
      Promise.all([
        Promise.resolve(1),
        Promise.resolve(2),
        Promise.resolve(3),
        Promise.resolve(4),
        Promise.resolve(5)
      ]);
    },
  });

  bench("Promise.race (5 resolved)", {
    run: () => {
      Promise.race([
        Promise.resolve(1),
        Promise.resolve(2),
        Promise.resolve(3),
        Promise.resolve(4),
        Promise.resolve(5)
      ]);
    },
  });

  bench("Promise.allSettled (5 mixed)", {
    run: () => {
      Promise.allSettled([
        Promise.resolve(1),
        Promise.reject("err"),
        Promise.resolve(3),
        Promise.reject("err2"),
        Promise.resolve(5)
      ]);
    },
  });

  bench("Promise.any (5 mixed)", {
    run: () => {
      Promise.any([
        Promise.reject("err1"),
        Promise.resolve(2),
        Promise.reject("err3"),
        Promise.resolve(4),
        Promise.resolve(5)
      ]);
    },
  });
});
