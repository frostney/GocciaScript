/*---
description: Microtask queue is drained after script execution so top-level .then callbacks fire
features: [Promise, microtasks]
---*/

test("top-level .then callback fires via post-execution drain", () => {
  const log = [];
  Promise.resolve(42).then((v) => log.push("then:" + v));
  return Promise.resolve().then(() => {
    expect(log).toEqual(["then:42"]);
  });
});

test("chained .then callbacks all fire during drain", () => {
  const log = [];
  Promise.resolve(1)
    .then((v) => { log.push("a:" + v); return v + 1; })
    .then((v) => { log.push("b:" + v); return v + 1; })
    .then((v) => log.push("c:" + v));
  return Promise.resolve()
    .then(() => {})
    .then(() => {})
    .then(() => {
      expect(log).toEqual(["a:1", "b:2", "c:3"]);
    });
});

test("reject .then callback fires via drain", () => {
  const log = [];
  Promise.reject("err").then(null, (e) => log.push("caught:" + e));
  return Promise.resolve()
    .then(() => {})
    .then(() => {
      expect(log).toEqual(["caught:err"]);
    });
});

test("multiple independent .then chains all drain", () => {
  const log = [];
  Promise.resolve("a").then((v) => log.push(v));
  Promise.resolve("b").then((v) => log.push(v));
  Promise.resolve("c").then((v) => log.push(v));
  return Promise.resolve()
    .then(() => {})
    .then(() => {
      expect(log).toEqual(["a", "b", "c"]);
    });
});

test(".then callback with closure over outer scope", () => {
  const prefix = "value";
  const log = [];
  Promise.resolve(99).then((v) => log.push(prefix + ":" + v));
  return Promise.resolve()
    .then(() => {})
    .then(() => {
      expect(log).toEqual(["value:99"]);
    });
});

test(".then callback that resolves another promise", () => {
  const log = [];
  Promise.resolve("outer").then((v) => {
    log.push("first:" + v);
    return Promise.resolve("inner").then((v2) => {
      log.push("second:" + v2);
    });
  });
  return Promise.resolve()
    .then(() => {})
    .then(() => {})
    .then(() => {})
    .then(() => {
      expect(log).toEqual(["first:outer", "second:inner"]);
    });
});
