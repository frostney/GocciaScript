/*---
description: Promise.prototype.then handles fulfillment and rejection
features: [Promise.prototype.then]
---*/

test("then with fulfillment handler", () => {
  return Promise.resolve(42).then((v) => {
    expect(v).toBe(42);
  });
});

test("then with rejection handler", () => {
  return Promise.reject("err").then(null, (e) => {
    expect(e).toBe("err");
  });
});

test("then returns a new promise", () => {
  const p1 = Promise.resolve(1);
  const p2 = p1.then((v) => v);
  expect(p2).not.toBe(p1);
});

test("then chaining with value transformation", () => {
  return Promise.resolve(1)
    .then((v) => v + 1)
    .then((v) => v * 2)
    .then((v) => {
      expect(v).toBe(4);
    });
});

test("then handler returning a promise chains correctly", () => {
  return Promise.resolve(1)
    .then((v) => Promise.resolve(v + 10))
    .then((v) => {
      expect(v).toBe(11);
    });
});

test("then handler throwing rejects the chain", () => {
  return Promise.resolve(1)
    .then((v) => { throw "error from then"; })
    .catch((e) => {
      expect(e).toBe("error from then");
    });
});

test("then with no fulfillment handler passes value through", () => {
  return Promise.resolve(42)
    .then()
    .then((v) => {
      expect(v).toBe(42);
    });
});

test("then with no rejection handler passes rejection through", () => {
  return Promise.reject("err")
    .then()
    .catch((e) => {
      expect(e).toBe("err");
    });
});

test("then fulfillment handler receives the resolved value", () => {
  const obj = { key: "value" };
  return Promise.resolve(obj).then((v) => {
    expect(v).toBe(obj);
    expect(v.key).toBe("value");
  });
});

test("then on rejected promise skips fulfillment handler", () => {
  const log = [];
  return Promise.reject("err").then(
    (v) => log.push("fulfilled: " + v),
    (e) => log.push("rejected: " + e)
  ).then(() => {
    expect(log).toEqual(["rejected: err"]);
  });
});

test("then on fulfilled promise skips rejection handler", () => {
  const log = [];
  return Promise.resolve("ok").then(
    (v) => log.push("fulfilled: " + v),
    (e) => log.push("rejected: " + e)
  ).then(() => {
    expect(log).toEqual(["fulfilled: ok"]);
  });
});

test("then handler returning rejected promise rejects the chain", () => {
  return Promise.resolve(1)
    .then((v) => Promise.reject("from handler"))
    .catch((e) => {
      expect(e).toBe("from handler");
    });
});

test("then with non-function onFulfilled passes value through", () => {
  return Promise.resolve(42)
    .then(99)
    .then((v) => {
      expect(v).toBe(42);
    });
});

test("then with non-function onRejected passes rejection through", () => {
  return Promise.reject("err")
    .then((v) => v, "not a function")
    .catch((e) => {
      expect(e).toBe("err");
    });
});

test("then handler with no return fulfills with undefined", () => {
  return Promise.resolve(42)
    .then((v) => { /* no return */ })
    .then((v) => {
      expect(v).toBeUndefined();
    });
});

test("onRejected returning a value fulfills the chain", () => {
  return Promise.reject("err").then(
    (v) => { throw new Error("should not run"); },
    (e) => "recovered"
  ).then((v) => {
    expect(v).toBe("recovered");
  });
});
