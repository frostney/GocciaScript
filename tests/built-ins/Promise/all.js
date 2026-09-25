/*---
description: Promise.all waits for all promises to resolve
features: [Promise.all]
---*/

test("Promise.all with empty array", () => {
  return Promise.all([]).then((v) => {
    expect(v).toEqual([]);
  });
});

test("Promise.all with all resolved", () => {
  return Promise.all([
    Promise.resolve(1),
    Promise.resolve(2),
    Promise.resolve(3)
  ]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all preserves order", () => {
  return Promise.all([
    Promise.resolve("a"),
    Promise.resolve("b"),
    Promise.resolve("c")
  ]).then((v) => {
    expect(v).toEqual(["a", "b", "c"]);
  });
});

test("Promise.all with non-promise values", () => {
  return Promise.all([1, 2, 3]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all rejects on first rejection", () => {
  return Promise.all([
    Promise.resolve(1),
    Promise.reject("err"),
    Promise.resolve(3)
  ]).catch((e) => {
    expect(e).toBe("err");
  });
});

test("Promise.all with mixed promises and values", () => {
  return Promise.all([
    1,
    Promise.resolve(2),
    3
  ]).then((v) => {
    expect(v).toEqual([1, 2, 3]);
  });
});

test("Promise.all with single element", () => {
  return Promise.all([Promise.resolve(42)]).then((v) => {
    expect(v).toEqual([42]);
  });
});

test("Promise.all with multiple rejections only catches first", () => {
  return Promise.all([
    Promise.reject("first"),
    Promise.reject("second"),
    Promise.reject("third")
  ]).catch((e) => {
    expect(e).toBe("first");
  });
});

test("Promise.all with string iterates characters", () => {
  return Promise.all("hello").then((result) => {
    expect(result).toEqual(["h", "e", "l", "l", "o"]);
  });
});

test("Promise.all with Set iterates values", () => {
  return Promise.all(new Set([1, 2, 3])).then((result) => {
    expect(result).toEqual([1, 2, 3]);
  });
});

test("Promise.all with Set of promises", () => {
  const s = new Set([Promise.resolve("a"), Promise.resolve("b")]);
  return Promise.all(s).then((result) => {
    expect(result).toEqual(["a", "b"]);
  });
});

test("Promise.all with Map iterates entries", () => {
  const m = new Map([["x", 1], ["y", 2]]);
  return Promise.all(m).then((result) => {
    expect(result).toEqual([["x", 1], ["y", 2]]);
  });
});

test("Promise.all accepts deferred thenable rejection", () => {
  const thenable = {
    then(resolve, reject) {
      Promise.resolve().then(() => reject("deferred"));
    }
  };
  return Promise.all([thenable]).then(() => {
    throw new Error("expected rejection");
  }, (reason) => {
    expect(reason).toBe("deferred");
  });
});

test("Promise.all ignores late thenable rejection after resolve", () => {
  const thenable = {
    then(resolve, reject) {
      Promise.resolve().then(() => {
        resolve(9);
        reject("late");
      });
    }
  };
  return Promise.all([thenable]).then((result) => {
    expect(result).toEqual([9]);
  });
});

test("Promise.all resolve element functions are anonymous built-ins", () => {
  let resolveElement;

  class NotPromise {
    constructor(executor) {
      executor(() => {}, () => {});
    }

    static resolve(value) {
      return value;
    }
  }

  Promise.all.call(NotPromise, [{
    then(resolve) {
      resolveElement = resolve;
    }
  }]);

  const nameDescriptor = Object.getOwnPropertyDescriptor(resolveElement, "name");
  const propertyNames = Object.getOwnPropertyNames(resolveElement);
  expect(nameDescriptor.value).toBe("");
  expect(nameDescriptor.writable).toBe(false);
  expect(nameDescriptor.enumerable).toBe(false);
  expect(nameDescriptor.configurable).toBe(true);
  expect(propertyNames.indexOf("name")).toBe(propertyNames.indexOf("length") + 1);
});

test("Promise.all resolve element functions ignore repeated calls", () => {
  let callCount = 0;

  class NotPromise {
    constructor(executor) {
      executor((values) => {
        callCount += 1;
        expect(values).toEqual(["first", "second"]);
      }, () => {
        throw new Error("unexpected rejection");
      });
    }

    static resolve(value) {
      return value;
    }
  }

  Promise.all.call(NotPromise, [
    {
      then(resolve) {
        resolve("first");
        resolve("bad");
      }
    },
    {
      then(resolve) {
        resolve("second");
        resolve("worse");
      }
    }
  ]);

  expect(callCount).toBe(1);
});

test("Promise.all rejects non-iterable arguments", async () => {
  for (const value of [42, null, undefined, true, { length: 2 }]) {
    try {
      await Promise.all(value);
      throw new Error("Expected rejection");
    } catch (error) {
      expect(error).toBeInstanceOf(TypeError);
    }
  }
});

test("Promise.all skips IteratorClose when an iterator result accessor throws", async () => {
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
      await Promise.all(iterable);
      throw new Error("Expected rejection");
    } catch (error) {
      expect(error).toBe(sentinel);
      expect(returnCalled).toBe(0);
    }
  }
});

// The capability triple comes out of a user subclass constructor, the resolve
// method out of a user getter, and both — along with the iterator and the
// shared element state — sit in native locals across the iterator's next and
// every thenable's then. Each of those is a GC safe point.
describe.runIf(typeof Goccia !== "undefined")("Promise.all under explicit GC", () => {
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

    return P.all([Promise.resolve(1), Promise.resolve(2)]).then((values) => {
      expect(values).toEqual([1, 2]);
    });
  });

  test("survives a collection inside the resolve getter", () => {
    class P extends Promise {
      static get resolve() {
        gcChurn();
        return (value) => Promise.resolve(value);
      }
    }

    return P.all([1, 2]).then((values) => {
      expect(values).toEqual([1, 2]);
    });
  });

  test("survives a collection inside the iterator's next", () => {
    const iterable = {
      [Symbol.iterator]() {
        let index = 0;
        return {
          next() {
            gcChurn();
            index++;
            if (index > 3) {
              return { done: true };
            }
            return { value: index, done: false };
          },
        };
      },
    };

    return Promise.all(iterable).then((values) => {
      expect(values).toEqual([1, 2, 3]);
    });
  });
});
