/*---
description: Promise microtask ordering follows the ECMAScript specification
features: [Promise, microtasks]
---*/

test("then callbacks run after synchronous code", () => {
  const log = [];
  const p = Promise.resolve(1).then((v) => log.push("then:" + v));
  log.push("sync");
  return p.then(() => {
    expect(log).toEqual(["sync", "then:1"]);
  });
});

test("multiple then callbacks execute in order", () => {
  const log = [];
  const p = Promise.resolve(1);
  p.then((v) => log.push("first:" + v));
  p.then((v) => log.push("second:" + v));
  const last = p.then((v) => log.push("third:" + v));
  return last.then(() => {
    expect(log).toEqual(["first:1", "second:1", "third:1"]);
  });
});

test("chained then callbacks execute in order", () => {
  const log = [];
  return Promise.resolve(1)
    .then((v) => { log.push("a:" + v); return v + 1; })
    .then((v) => { log.push("b:" + v); return v + 1; })
    .then((v) => { log.push("c:" + v); })
    .then(() => {
      expect(log).toEqual(["a:1", "b:2", "c:3"]);
    });
});

test("catch then chain ordering", () => {
  const log = [];
  return Promise.reject("err")
    .catch((e) => { log.push("catch:" + e); return "recovered"; })
    .then((v) => { log.push("then:" + v); })
    .then(() => {
      expect(log).toEqual(["catch:err", "then:recovered"]);
    });
});

test("nested promise resolution ordering", () => {
  const log = [];
  return Promise.resolve("outer").then((v) => {
    log.push("outer:" + v);
    return Promise.resolve("inner").then((v2) => {
      log.push("inner:" + v2);
    });
  }).then(() => {
    expect(log).toEqual(["outer:outer", "inner:inner"]);
  });
});

test("interleaved promise chains", () => {
  const log = [];
  const p1 = Promise.resolve(1).then((v) => log.push("a:" + v));
  const p2 = Promise.resolve(2).then((v) => log.push("b:" + v));
  return Promise.all([p1, p2]).then(() => {
    expect(log).toEqual(["a:1", "b:2"]);
  });
});

test("thenable adoption defers settlement by one microtask tick", () => {
  const log = [];
  const inner = Promise.resolve("v");
  const outer = new Promise((resolve) => resolve(inner));
  outer.then(() => log.push("B"));
  Promise.resolve().then(() => log.push("A"));
  return outer.then(() => {
    expect(log).toEqual(["A", "B"]);
  });
});

test("resolve(fulfilledPromise) does not settle outer synchronously", () => {
  const log = [];
  const fulfilled = Promise.resolve(42);
  const outer = new Promise((resolve) => {
    resolve(fulfilled);
    log.push("executor-done");
  });
  outer.then((v) => log.push("then:" + v));
  log.push("sync");
  return outer.then(() => {
    expect(log).toEqual(["executor-done", "sync", "then:42"]);
  });
});

test("resolve(rejectedPromise) defers rejection by one tick", () => {
  const log = [];
  const rejected = Promise.reject("err");
  const outer = new Promise((resolve) => resolve(rejected));
  outer.catch((e) => log.push("catch:" + e));
  Promise.resolve().then(() => log.push("A"));
  return outer.catch(() => {
    expect(log).toEqual(["A", "catch:err"]);
  });
});
