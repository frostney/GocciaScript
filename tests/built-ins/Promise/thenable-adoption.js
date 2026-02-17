/*---
description: Promise thenable adoption correctly chains through pending and settled promises
features: [Promise, thenable]
---*/

test("resolve with pending promise waits for fulfillment", () => {
  let innerResolve;
  const inner = new Promise((r) => { innerResolve = r; });
  const outer = Promise.resolve(inner);
  innerResolve(42);
  return outer.then((v) => {
    expect(v).toBe(42);
  });
});

test("resolve with pending promise waits for rejection", () => {
  let innerReject;
  const inner = new Promise((_, r) => { innerReject = r; });
  const outer = Promise.resolve(inner);
  innerReject("fail");
  return outer.catch((e) => {
    expect(e).toBe("fail");
  });
});

test("constructor resolve with pending promise adopts fulfillment", () => {
  let innerResolve;
  const inner = new Promise((r) => { innerResolve = r; });
  const outer = new Promise((resolve) => {
    resolve(inner);
  });
  innerResolve("adopted");
  return outer.then((v) => {
    expect(v).toBe("adopted");
  });
});

test("constructor resolve with pending promise adopts rejection", () => {
  let innerReject;
  const inner = new Promise((_, r) => { innerReject = r; });
  const outer = new Promise((resolve) => {
    resolve(inner);
  });
  innerReject("adopted error");
  return outer.catch((e) => {
    expect(e).toBe("adopted error");
  });
});

test("chained thenable adoption (promise resolving to promise resolving to value)", () => {
  const inner = Promise.resolve(42);
  const middle = Promise.resolve(inner);
  return Promise.resolve(middle).then((v) => {
    expect(v).toBe(42);
  });
});

test("then handler returning pending promise that later fulfills", () => {
  let innerResolve;
  const inner = new Promise((r) => { innerResolve = r; });
  const chain = Promise.resolve(1).then((v) => {
    innerResolve(v + 10);
    return inner;
  });
  return chain.then((v) => {
    expect(v).toBe(11);
  });
});

test("then handler returning pending promise that later rejects", () => {
  let innerReject;
  const inner = new Promise((_, r) => { innerReject = r; });
  const chain = Promise.resolve(1).then((v) => {
    innerReject("handler error");
    return inner;
  });
  return chain.catch((e) => {
    expect(e).toBe("handler error");
  });
});

test("resolve with fulfilled promise adopts its value", () => {
  const fulfilled = Promise.resolve("original");
  return new Promise((resolve) => {
    resolve(fulfilled);
  }).then((v) => {
    expect(v).toBe("original");
  });
});

test("resolve with rejected promise adopts its reason", () => {
  const rejected = Promise.reject("original error");
  return new Promise((resolve) => {
    resolve(rejected);
  }).catch((e) => {
    expect(e).toBe("original error");
  });
});
