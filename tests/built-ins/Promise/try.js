describe("Promise.try", () => {
  test("resolves with return value of synchronous function", () => {
    const p = Promise.try(() => 42);
    p.then((value) => {
      expect(value).toBe(42);
    });
  });

  test("rejects with thrown error from synchronous function", () => {
    const p = Promise.try(() => {
      throw new Error("oops");
    });
    p.catch((err) => {
      expect(err.message).toBe("oops");
    });
  });

  test("resolves with promise from async function", () => {
    const p = Promise.try(() => Promise.resolve(99));
    p.then((value) => {
      expect(value).toBe(99);
    });
  });
});

describe.runIf(typeof Goccia !== "undefined")("Promise.try across a collection", () => {
  test("settles its promise when the callback collects and returns", async () => {
    const value = await Promise.try((a, b) => {
      Goccia.gc();
      return a + b;
    }, 20, 22);
    expect(value).toBe(42);
  });

  test("rejects its promise when the callback collects and throws", async () => {
    const promise = Promise.try(() => {
      Goccia.gc();
      throw new TypeError("boom");
    });
    Goccia.gc();
    await expect(promise).rejects.toThrow("boom");
  });

  test("settles a subclass promise when the constructor collects", async () => {
    class CollectingPromise extends Promise {
      constructor(executor) {
        Goccia.gc();
        super(executor);
      }
    }
    const value = await CollectingPromise.try(() => {
      Goccia.gc();
      return "done";
    });
    expect(value).toBe("done");
  });
});
