/*---
description: Promise.prototype.finally runs callback regardless of outcome
features: [Promise.prototype.finally]
---*/

test("finally runs on fulfilled promise", () => {
  const log = [];
  return Promise.resolve(42)
    .finally(() => log.push("finally"))
    .then((v) => {
      log.push(v);
      expect(log).toEqual(["finally", 42]);
    });
});

test("finally runs on rejected promise", () => {
  const log = [];
  return Promise.reject("err")
    .finally(() => log.push("finally"))
    .catch((e) => {
      log.push(e);
      expect(log).toEqual(["finally", "err"]);
    });
});

test("finally preserves fulfillment value", () => {
  return Promise.resolve("original")
    .finally(() => "ignored")
    .then((v) => {
      expect(v).toBe("original");
    });
});

test("finally preserves rejection reason", () => {
  return Promise.reject("original")
    .finally(() => "ignored")
    .catch((e) => {
      expect(e).toBe("original");
    });
});

test("finally throwing overrides the outcome", () => {
  return Promise.resolve(42)
    .finally(() => { throw "finally error"; })
    .catch((e) => {
      expect(e).toBe("finally error");
    });
});

test("finally with no callback passes through", () => {
  return Promise.resolve(42)
    .finally()
    .then((v) => {
      expect(v).toBe(42);
    });
});
