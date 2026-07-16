/*---
description: Promise.any resolves with the first fulfilled promise
features: [Promise.any]
---*/

test("Promise.any with first fulfilled", () => {
  return Promise.any([
    Promise.resolve("first"),
    Promise.resolve("second")
  ]).then((v) => {
    expect(v).toBe("first");
  });
});

test("Promise.any skips rejections", () => {
  return Promise.any([
    Promise.reject("err1"),
    Promise.resolve("ok"),
    Promise.reject("err2")
  ]).then((v) => {
    expect(v).toBe("ok");
  });
});

test("Promise.any with all rejected creates AggregateError", () => {
  return Promise.any([
    Promise.reject("err1"),
    Promise.reject("err2")
  ]).catch((e) => {
    expect(e.name).toBe("AggregateError");
    expect(e.message).toBe("All promises were rejected");
    expect(e.errors).toEqual(["err1", "err2"]);
  });
});

test("Promise.any with empty array rejects with AggregateError", () => {
  return Promise.any([]).catch((e) => {
    expect(e.name).toBe("AggregateError");
  });
});

test("Promise.any with non-promise values", () => {
  return Promise.any([1, 2, 3]).then((v) => {
    expect(v).toBe(1);
  });
});

test("Promise.any with single rejected creates AggregateError", () => {
  return Promise.any([Promise.reject("only")]).catch((e) => {
    expect(e.name).toBe("AggregateError");
    expect(e.errors).toEqual(["only"]);
  });
});

test("Promise.any AggregateError preserves error order", () => {
  return Promise.any([
    Promise.reject("a"),
    Promise.reject("b"),
    Promise.reject("c")
  ]).catch((e) => {
    expect(e.errors).toEqual(["a", "b", "c"]);
  });
});

test("Promise.any rejects non-iterable arguments with TypeError", async () => {
  for (const value of [42, null, undefined]) {
    try {
      await Promise.any(value);
      throw new Error("Expected rejection");
    } catch (error) {
      expect(error).toBeInstanceOf(TypeError);
    }
  }
});

test("Promise.any skips IteratorClose when an iterator result accessor throws", async () => {
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
      await Promise.any(iterable);
      throw new Error("Expected rejection");
    } catch (error) {
      expect(error).toBe(sentinel);
      expect(returnCalled).toBe(0);
    }
  }
});
