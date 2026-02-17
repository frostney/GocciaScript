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
  return new Promise((resolve, reject) => {
    resolve(pending);
    reject("should be ignored");
  }).catch(() => {
    throw "promise should not have been rejected";
  });
});

test("resolve after resolve with pending promise is ignored", () => {
  const pending = new Promise(() => {});
  return new Promise((resolve, reject) => {
    resolve(pending);
    resolve("should be ignored");
  }).catch(() => {
    throw "promise should not have been rejected";
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
