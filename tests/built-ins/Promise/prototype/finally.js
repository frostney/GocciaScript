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

test("finally returning rejected promise overrides fulfillment", () => {
  return Promise.resolve(42)
    .finally(() => Promise.reject("override"))
    .then(
      (v) => { throw new Error("should not fulfill"); },
      (e) => { expect(e).toBe("override"); }
    );
});

test("finally returning rejected promise overrides rejection", () => {
  return Promise.reject("original")
    .finally(() => Promise.reject("override"))
    .catch((e) => {
      expect(e).toBe("override");
    });
});

test("finally waits for returned promise before continuing chain", () => {
  const log = [];
  return Promise.resolve("original")
    .finally(() => {
      return Promise.resolve("inner").then((v) => {
        log.push("inner:" + v);
      });
    })
    .then((v) => {
      log.push("outer:" + v);
      expect(log).toEqual(["inner:inner", "outer:original"]);
    });
});

test("finally returning fulfilled promise preserves original value", () => {
  return Promise.resolve("original")
    .finally(() => Promise.resolve("ignored"))
    .then((v) => {
      expect(v).toBe("original");
    });
});

test("finally returning fulfilled promise preserves rejection reason", () => {
  return Promise.reject("original")
    .finally(() => Promise.resolve("ignored"))
    .catch((e) => {
      expect(e).toBe("original");
    });
});

describe.runIf(typeof Goccia !== "undefined")("finally across a collection", () => {
  test("settles on a subclass whose constructor collects", async () => {
    class CollectingPromise extends Promise {
      constructor(executor) {
        Goccia.gc();
        super(executor);
      }
    }
    const value = await CollectingPromise.resolve(5).finally(() => {
      Goccia.gc();
    });
    expect(value).toBe(5);
  });

  test("keeps the rejection on a subclass whose constructor collects", async () => {
    class CollectingPromise extends Promise {
      constructor(executor) {
        Goccia.gc();
        super(executor);
      }
    }
    const promise = CollectingPromise.reject(new RangeError("kept")).finally(() => {
      Goccia.gc();
    });
    await expect(promise).rejects.toThrow("kept");
  });

  test("uses a species constructor that nothing else holds", async () => {
    const makeSpecies = () =>
      class FreshPromise extends Promise {
        constructor(executor) {
          Goccia.gc();
          super(executor);
        }
      };
    class SpeciesPromise extends Promise {
      static get [Symbol.species]() {
        return makeSpecies();
      }
    }
    const value = await SpeciesPromise.resolve(7)
      .then((v) => v + 1)
      .finally(() => Goccia.gc());
    expect(value).toBe(8);
  });
});
