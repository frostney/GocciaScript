/*---
description: Promise.race resolves or rejects with the first settled promise
features: [Promise.race]
---*/

test("Promise.race with first fulfilled", () => {
  return Promise.race([
    Promise.resolve("first"),
    Promise.resolve("second")
  ]).then((v) => {
    expect(v).toBe("first");
  });
});

test("Promise.race with first rejected", () => {
  return Promise.race([
    Promise.reject("err"),
    Promise.resolve("ok")
  ]).catch((e) => {
    expect(e).toBe("err");
  });
});

test("Promise.race with non-promise values", () => {
  return Promise.race([1, 2, 3]).then((v) => {
    expect(v).toBe(1);
  });
});

test("Promise.race with single element", () => {
  return Promise.race([Promise.resolve(42)]).then((v) => {
    expect(v).toBe(42);
  });
});

test("Promise.race with mixed resolved and rejected", () => {
  return Promise.race([
    Promise.resolve("winner"),
    Promise.reject("loser")
  ]).then((v) => {
    expect(v).toBe("winner");
  });
});

test("Promise.race with empty array returns forever-pending promise", () => {
  const p = Promise.race([]);
  let settled = false;
  p.then(() => { settled = true; });
  p.catch(() => { settled = true; });
  return Promise.resolve().then(() => {
    expect(settled).toBe(false);
  });
});

test("Promise.race with single rejected element", () => {
  return Promise.race([Promise.reject("only")]).catch((e) => {
    expect(e).toBe("only");
  });
});

test("Promise.race rejects non-iterable arguments", async () => {
  for (const value of [42, null, undefined]) {
    try {
      await Promise.race(value);
      throw new Error("Expected rejection");
    } catch (error) {
      expect(error).toBeInstanceOf(TypeError);
    }
  }
});

test("Promise.race skips IteratorClose when an iterator result accessor throws", async () => {
  for (const property of ["done", "value"]) {
    let returnCalled = 0;
    const sentinel = new Error(property + "-boom");
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            if (property === "done") {
              return {
                get done() {
                  throw sentinel;
                },
                value: 1,
              };
            }
            return {
              done: false,
              get value() {
                throw sentinel;
              },
            };
          },
          return() {
            returnCalled++;
            return { done: true };
          },
        };
      },
    };

    try {
      await Promise.race(iterable);
      throw new Error("Expected rejection");
    } catch (error) {
      expect(error).toBe(sentinel);
      expect(returnCalled).toBe(0);
    }
  }
});

// Same rooting exposure as Promise.all: see that file for the rationale.
describe.runIf(typeof Goccia !== "undefined")("Promise.race under explicit GC", () => {
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

  test("survives a collection inside a subclass constructor", () => {
    class P extends Promise {
      constructor(executor) {
        gcChurn();
        super(executor);
      }
    }

    return P.race([Promise.resolve(7)]).then((value) => {
      expect(value).toBe(7);
    });
  });
});
