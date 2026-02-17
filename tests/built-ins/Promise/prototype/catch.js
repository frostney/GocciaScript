/*---
description: Promise.prototype.catch handles rejections
features: [Promise.prototype.catch]
---*/

test("catch handles rejected promise", () => {
  return Promise.reject("error").catch((e) => {
    expect(e).toBe("error");
  });
});

test("catch returns a new promise", () => {
  const p1 = Promise.reject("err");
  const p2 = p1.catch((e) => e);
  expect(p2).not.toBe(p1);
});

test("catch recovers from rejection", () => {
  return Promise.reject("err")
    .catch((e) => "recovered")
    .then((v) => {
      expect(v).toBe("recovered");
    });
});

test("catch handler throwing rejects the chain", () => {
  return Promise.reject("err1")
    .catch((e) => { throw "err2"; })
    .catch((e) => {
      expect(e).toBe("err2");
    });
});

test("catch is skipped for fulfilled promises", () => {
  return Promise.resolve(42)
    .catch((e) => "should not run")
    .then((v) => {
      expect(v).toBe(42);
    });
});

test("catch with chained then", () => {
  return Promise.reject("err")
    .catch((e) => 100)
    .then((v) => {
      expect(v).toBe(100);
    });
});

test("multiple catch handlers on same promise all fire in order", () => {
  const log = [];
  const p = Promise.reject("err");
  p.catch((e) => log.push("first:" + e));
  p.catch((e) => log.push("second:" + e));
  const last = p.catch((e) => log.push("third:" + e));
  return last.then(() => {
    expect(log).toEqual(["first:err", "second:err", "third:err"]);
  });
});
