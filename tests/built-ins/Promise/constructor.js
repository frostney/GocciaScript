/*---
description: Promise constructor creates Promise objects correctly
features: [Promise]
---*/

test("Promise constructor with resolve", () => {
  let resolved = false;
  const p = new Promise((resolve, reject) => {
    resolved = true;
    resolve(42);
  });
  expect(resolved).toBe(true);
});

test("Promise constructor with reject", () => {
  let rejected = false;
  const p = new Promise((resolve, reject) => {
    rejected = true;
    reject("error");
  });
  expect(rejected).toBe(true);
});

test("executor runs synchronously", () => {
  const log = [];
  log.push("before");
  const p = new Promise((resolve) => {
    log.push("executor");
    resolve();
  });
  log.push("after");
  expect(log).toEqual(["before", "executor", "after"]);
});

test("executor receives resolve and reject functions", () => {
  let resolveType;
  let rejectType;
  const p = new Promise((resolve, reject) => {
    resolveType = typeof resolve;
    rejectType = typeof reject;
  });
  expect(resolveType).toBe("function");
  expect(rejectType).toBe("function");
});

test("double resolve is ignored", () => {
  return new Promise((resolve) => {
    resolve(1);
    resolve(2);
  }).then((v) => {
    expect(v).toBe(1);
  });
});

test("resolve after reject is ignored", () => {
  return new Promise((resolve, reject) => {
    reject("err");
    resolve("ok");
  }).catch((e) => {
    expect(e).toBe("err");
  });
});

test("reject after resolve is ignored", () => {
  return new Promise((resolve, reject) => {
    resolve("ok");
    reject("err");
  }).then((v) => {
    expect(v).toBe("ok");
  });
});

test("executor throwing rejects the promise", () => {
  return new Promise(() => {
    throw "executor error";
  }).catch((e) => {
    expect(e).toBe("executor error");
  });
});

test("Promise constructor without executor throws TypeError", () => {
  let caught = false;
  try {
    const p = new Promise();
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("Promise constructor with non-function throws TypeError", () => {
  let caught = false;
  try {
    const p = new Promise(42);
  } catch (e) {
    caught = true;
    expect(e.name).toBe("TypeError");
  }
  expect(caught).toBe(true);
});

test("reject after resolve with pending promise is ignored", () => {
  const pending = new Promise(() => {});
  let rejectHandlerCalled = false;
  const p = new Promise((resolve, reject) => {
    resolve(pending);
    reject("should be ignored");
  });
  // Register a catch on the pending-adopted promise to detect if reject() leaked through
  p.catch(() => { rejectHandlerCalled = true; });
  // Return a separate settled chain so the test itself settles.
  // After microtask drain, if reject() was incorrectly applied, the catch
  // handler above would have fired (its microtask enqueued before ours).
  return Promise.resolve().then(() => {
    expect(rejectHandlerCalled).toBe(false);
  });
});

test("resolve after resolve with pending promise is ignored", () => {
  const pending = new Promise(() => {});
  let rejectHandlerCalled = false;
  const p = new Promise((resolve, reject) => {
    resolve(pending);
    resolve("should be ignored");
  });
  // Verify the second resolve() did not cause a rejection
  p.catch(() => { rejectHandlerCalled = true; });
  return Promise.resolve().then(() => {
    expect(rejectHandlerCalled).toBe(false);
  });
});

test("resolve with settled promise then reject is ignored", () => {
  return new Promise((resolve, reject) => {
    resolve(Promise.resolve("adopted"));
    reject("should be ignored");
  }).then((v) => {
    expect(v).toBe("adopted");
  });
});

test("resolving a promise with itself rejects with TypeError", () => {
  let resolve;
  const p = new Promise((r) => { resolve = r; });
  resolve(p);
  return p.catch((e) => {
    expect(e.name).toBe("TypeError");
    expect(e.message).toBe("Chaining cycle detected for promise");
  });
});

test("exception after resolve in executor is ignored", () => {
  return new Promise((resolve) => {
    resolve(42);
    throw "should be ignored";
  }).then((v) => {
    expect(v).toBe(42);
  });
});

test("exception after reject in executor is ignored", () => {
  return new Promise((resolve, reject) => {
    reject("err");
    throw "should be ignored";
  }).catch((e) => {
    expect(e).toBe("err");
  });
});

test("exception after resolve with pending promise is ignored", () => {
  let innerResolve;
  const inner = new Promise((r) => { innerResolve = r; });
  const outer = new Promise((resolve) => {
    resolve(inner);
    throw "should be ignored";
  });
  innerResolve("adopted value");
  return outer.then((v) => {
    expect(v).toBe("adopted value");
  });
});

test("exception after resolve with pending promise does not reject", () => {
  const pending = new Promise(() => {});
  let rejectHandlerCalled = false;
  const p = new Promise((resolve) => {
    resolve(pending);
    throw "should be ignored";
  });
  // Verify the throw after resolve(pending) did not reject the promise
  p.catch(() => { rejectHandlerCalled = true; });
  return Promise.resolve().then(() => {
    expect(rejectHandlerCalled).toBe(false);
  });
});

test("Promise.prototype is an object", () => {
  expect(typeof Promise.prototype).toBe("object");
});

test("new Promise instanceof Promise", () => {
  const p = new Promise((resolve) => resolve(1));
  expect(p instanceof Promise).toBe(true);
});

test("Promise.resolve() instanceof Promise", () => {
  const p = Promise.resolve(42);
  expect(p instanceof Promise).toBe(true);
});

test("Promise.reject() instanceof Promise", () => {
  const p = Promise.reject("err");
  p.catch(() => {}); // prevent unhandled rejection
  expect(p instanceof Promise).toBe(true);
});

test("Promise.prototype.constructor is Promise", () => {
  expect(Promise.prototype.constructor).toBe(Promise);
});

test("instance constructor is Promise", () => {
  const p = new Promise((resolve) => resolve(1));
  expect(p.constructor).toBe(Promise);
});

test("Promise.prototype has then method", () => {
  expect(typeof Promise.prototype.then).toBe("function");
});

test("Promise.prototype has catch method", () => {
  expect(typeof Promise.prototype.catch).toBe("function");
});

test("Promise.prototype has finally method", () => {
  expect(typeof Promise.prototype.finally).toBe("function");
});
