describe("for-await-of iterator close", () => {
  test("calls async iterator return on break", async () => {
    const log = [];
    const asyncIter = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i++;
            log.push("next:" + i);
            if (i <= 3) return Promise.resolve({ value: i, done: false });
            return Promise.resolve({ value: undefined, done: true });
          },
          return() {
            log.push("return");
            return Promise.resolve({ done: true });
          }
        };
      }
    };

    await (async () => {
      for await (const x of asyncIter) {
        if (x === 2) break;
      }
    })();

    expect(log.join(",")).toBe("next:1,next:2,return");
  });

  test("calls async iterator return on return", async () => {
    let returnCalled = 0;
    const asyncIter = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: 1, done: false });
          },
          return() {
            returnCalled++;
            return Promise.resolve({ done: true });
          }
        };
      }
    };

    const result = await (async () => {
      for await (const x of asyncIter) {
        return "done";
      }
      return "missed";
    })();

    expect(result).toBe("done");
    expect(returnCalled).toBe(1);
  });

  test("calls async iterator return on throw in body", async () => {
    let returnCalled = 0;
    let caughtMessage = "";
    const asyncIter = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: 1, done: false });
          },
          return() {
            returnCalled++;
            return Promise.resolve({ done: true });
          }
        };
      }
    };

    try {
      await (async () => {
        for await (const x of asyncIter) {
          throw new Error("body-threw");
        }
      })();
    } catch (e) {
      caughtMessage = e.message;
    }

    expect(returnCalled).toBe(1);
    expect(caughtMessage).toBe("body-threw");
  });

  test("preserves original error when return also throws", async () => {
    let caughtMessage = "";
    const asyncIter = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: 1, done: false });
          },
          return() {
            throw new Error("return-threw");
          }
        };
      }
    };

    try {
      await (async () => {
        for await (const x of asyncIter) {
          throw new Error("body-threw");
        }
      })();
    } catch (e) {
      caughtMessage = e.message;
    }

    expect(caughtMessage).toBe("body-threw");
  });

  test("does not call async iterator return for continue", async () => {
    let returnCalled = 0;
    const asyncIter = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i++;
            return i <= 2
              ? Promise.resolve({ value: i, done: false })
              : Promise.resolve({ done: true });
          },
          return() {
            returnCalled++;
            return Promise.resolve({ done: true });
          }
        };
      }
    };

    const result = [];
    await (async () => {
      for await (const x of asyncIter) {
        if (x === 1) continue;
        result.push(x);
      }
    })();

    expect(result).toEqual([2]);
    expect(returnCalled).toBe(0);
  });

  test("does not call return when iterator has no return method", async () => {
    const log = [];
    const asyncIter = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i++;
            log.push("next:" + i);
            return Promise.resolve(
              i <= 3 ? { value: i, done: false } : { done: true }
            );
          }
        };
      }
    };

    await (async () => {
      for await (const x of asyncIter) {
        if (x === 2) break;
      }
    })();

    expect(log.join(",")).toBe("next:1,next:2");
  });

  test("calls return on sync iterator fallback via Symbol.iterator", async () => {
    let returnCalled = 0;
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i++;
            return i <= 3 ? { value: i, done: false } : { done: true };
          },
          return() {
            returnCalled++;
            return { done: true };
          }
        };
      }
    };

    await (async () => {
      for await (const x of iterable) {
        if (x === 2) break;
      }
    })();

    expect(returnCalled).toBe(1);
  });

  test("calls return on sync iterator fallback when body throws", async () => {
    let returnCalled = 0;
    let caughtMessage = "";
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            return { value: 1, done: false };
          },
          return() {
            returnCalled++;
            return { done: true };
          }
        };
      }
    };

    try {
      await (async () => {
        for await (const x of iterable) {
          throw new Error("body-threw");
        }
      })();
    } catch (e) {
      caughtMessage = e.message;
    }

    expect(returnCalled).toBe(1);
    expect(caughtMessage).toBe("body-threw");
  });
});
