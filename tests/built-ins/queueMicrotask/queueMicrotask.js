/*---
description: queueMicrotask global function
features: [queueMicrotask]
---*/

test("queueMicrotask is a function", () => {
  expect(typeof queueMicrotask).toBe("function");
});

test("queueMicrotask returns undefined", () => {
  const result = queueMicrotask(() => {});
  expect(result).toBe(undefined);
});

test("queueMicrotask executes callback asynchronously", () => {
  const log = [];
  log.push("before");
  queueMicrotask(() => {
    log.push("microtask");
  });
  log.push("after");

  return Promise.resolve().then(() => {
    expect(log).toEqual(["before", "after", "microtask"]);
  });
});

test("queueMicrotask callbacks run in order", () => {
  const log = [];
  queueMicrotask(() => log.push("first"));
  queueMicrotask(() => log.push("second"));
  queueMicrotask(() => log.push("third"));

  return Promise.resolve().then(() => {
    expect(log).toEqual(["first", "second", "third"]);
  });
});

test("queueMicrotask interleaves with promise microtasks", () => {
  const log = [];
  Promise.resolve().then(() => log.push("promise-1"));
  queueMicrotask(() => log.push("microtask-1"));
  Promise.resolve().then(() => log.push("promise-2"));
  queueMicrotask(() => log.push("microtask-2"));

  return Promise.resolve().then(() => {
    return Promise.resolve();
  }).then(() => {
    expect(log).toEqual([
      "promise-1", "microtask-1", "promise-2", "microtask-2"
    ]);
  });
});

test("queueMicrotask callback can enqueue more microtasks", () => {
  const log = [];
  queueMicrotask(() => {
    log.push("outer");
    queueMicrotask(() => {
      log.push("inner");
    });
  });

  return Promise.resolve().then(() => {
    return Promise.resolve();
  }).then(() => {
    expect(log).toEqual(["outer", "inner"]);
  });
});

test("queueMicrotask runs before next promise chain step", () => {
  const log = [];
  return Promise.resolve().then(() => {
    log.push("then-1");
    queueMicrotask(() => log.push("queued-from-then"));
  }).then(() => {
    log.push("then-2");
    expect(log).toEqual(["then-1", "queued-from-then", "then-2"]);
  });
});

test("queueMicrotask with no arguments throws TypeError", () => {
  expect(() => queueMicrotask()).toThrow(TypeError);
});

test("queueMicrotask with non-function throws TypeError", () => {
  expect(() => queueMicrotask(42)).toThrow(TypeError);
  expect(() => queueMicrotask("string")).toThrow(TypeError);
  expect(() => queueMicrotask(null)).toThrow(TypeError);
  expect(() => queueMicrotask(undefined)).toThrow(TypeError);
  expect(() => queueMicrotask({})).toThrow(TypeError);
  expect(() => queueMicrotask(true)).toThrow(TypeError);
});

test("multiple queueMicrotask with promise resolution", () => {
  const log = [];
  const p = new Promise((resolve) => {
    queueMicrotask(() => {
      log.push("microtask-before-resolve");
      resolve("done");
    });
  });

  return p.then((value) => {
    log.push("resolved:" + value);
    expect(log).toEqual(["microtask-before-resolve", "resolved:done"]);
  });
});
