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

// Reading then off a thenable runs a user getter, which is a GC safe point.
// The thenable and both reaction handlers are native locals at that moment.
describe.runIf(typeof Goccia !== "undefined")("thenable adoption under explicit GC", () => {
  // A bare gc() usually leaves the freed slot readable; the allocation churn
  // afterwards is what makes a collected value observable.
  const gcChurn = () => {
    Goccia.gc();
    let total = 0;
    for (const i of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
      const scratch = { a: i * 7.5, b: [i, i + 1], c: "x" + i };
      total += scratch.a + scratch.b[0];
    }
    return total;
  };

  test("survives a collection inside the then getter", () => {
    const thenable = {
      get then() {
        gcChurn();
        return (resolve) => resolve({ deep: [1, 2, 3], label: "ok" });
      },
    };

    return Promise.resolve(thenable).then((value) => {
      expect(value.label).toBe("ok");
      expect(value.deep).toEqual([1, 2, 3]);
    });
  });
});
