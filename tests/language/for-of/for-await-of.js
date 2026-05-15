/*---
description: for-await-of with async iterables
features: [for-of, async-await]
---*/

describe("for-await-of", () => {
  test("iterates over async iterable", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return Promise.resolve({ value: i, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    const fn = async () => {
      const result = [];
      for await (const item of asyncIterable) {
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([1, 2, 3]);
    });
  });

  test("for-await-of falls back to sync iterable", () => {
    const fn = async () => {
      const result = [];
      for await (const item of [10, 20, 30]) {
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([10, 20, 30]);
    });
  });

  test("for-await-of awaits Promise values from sync iterator", () => {
    const fn = async () => {
      const promises = [Promise.resolve(1), Promise.resolve(2), Promise.resolve(3)];
      const result = [];
      for await (const item of promises) {
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([1, 2, 3]);
    });
  });

  test("for-await-of awaits Promise values from custom sync iterator results", async () => {
    const source = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return { value: Promise.resolve(i * 2), done: false };
            }
            return { value: Promise.resolve(99), done: true };
          }
        };
      }
    };

    const result = [];
    for await (const item of source) {
      result.push(item);
    }

    expect(result).toEqual([2, 4, 6]);
  });

  test("break in for-await-of", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            return Promise.resolve({
              value: i,
              done: i > 10
            });
          }
        };
      }
    };

    const fn = async () => {
      const result = [];
      for await (const item of asyncIterable) {
        if (item === 3) {
          break;
        }
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([1, 2]);
    });
  });

  test("non-iterable values reject with TypeError", async () => {
    await expect((async () => {
      for await (const x of null) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of undefined) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of 42) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of true) {
      }
    })()).rejects.toThrow(TypeError);
  });

  test("non-callable async iterator property rejects with TypeError", async () => {
    const source = {
      [Symbol.asyncIterator]: {
        next() {
          return Promise.resolve({ value: 1, done: false });
        },
      },
    };

    await expect((async () => {
      for await (const x of source) {
      }
    })()).rejects.toThrow(TypeError);
  });

  test("non-callable async iterator property rejects before sync fallback", async () => {
    const source = {
      [Symbol.asyncIterator]: 1,
      [Symbol.iterator]() {
        return [1][Symbol.iterator]();
      },
    };

    await expect((async () => {
      for await (const x of source) {
      }
    })()).rejects.toThrow(TypeError);
  });

  test("null async iterator property falls back to sync iterator", async () => {
    const source = {
      [Symbol.asyncIterator]: null,
      [Symbol.iterator]() {
        return [1, 2][Symbol.iterator]();
      },
    };
    const result = [];

    for await (const x of source) {
      result.push(x);
    }

    expect(result).toEqual([1, 2]);
  });

  test("invalid async iterator result rejects with TypeError", async () => {
    const primitiveIterator = {
      [Symbol.asyncIterator]() {
        return 1;
      },
    };
    const missingNext = {
      [Symbol.asyncIterator]() {
        return {};
      },
    };
    const nextNotCallable = {
      [Symbol.asyncIterator]() {
        return { next: 1 };
      },
    };

    await expect((async () => {
      for await (const x of primitiveIterator) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of missingNext) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of nextNotCallable) {
      }
    })()).rejects.toThrow(TypeError);
  });

  test("invalid sync iterator result rejects with TypeError through async wrapper", async () => {
    const primitiveResult = {
      [Symbol.iterator]() {
        return {
          next() {
            return Symbol("not-object");
          },
        };
      },
    };

    const iterate = async () => {
      for await (const x of primitiveResult) {
      }
    };

    await expect(iterate()).rejects.toThrow(TypeError);
  });

  test("rejected Promise values from sync iterables throw", async () => {
    const values = [Promise.resolve(1), Promise.reject("fail"), Promise.resolve(3)];
    const result = [];

    try {
      for await (const value of values) {
        result.push(value);
      }
    } catch (e) {
      expect(e).toBe("fail");
      expect(result).toEqual([1]);
      return;
    }

    expect(true).toBe(false);
  });

  test("async iterables can reject or finish immediately", async () => {
    const rejectingIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i === 2) {
              return Promise.reject("iteration error");
            }
            if (i <= 3) {
              return Promise.resolve({ value: i, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    const emptyIterable = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    const result = [];
    try {
      for await (const value of rejectingIterable) {
        result.push(value);
      }
      throw new Error("expected rejection");
    } catch (e) {
      expect(e).toBe("iteration error");
      expect(result).toEqual([1]);
    }

    const emptyResult = [];
    for await (const value of emptyIterable) {
      emptyResult.push(value);
    }
    expect(emptyResult).toEqual([]);
  });

  test("for-await-of falls back to sync iterables and mixed values", async () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);

    const set = new Set([10, 20, 30]);
    const mixed = [];
    const entries = [];
    const setValues = [];

    for await (const value of [Promise.resolve(1), 2, Promise.resolve(3), 4]) {
      mixed.push(value);
    }

    for await (const entry of map) {
      entries.push(entry);
    }

    for await (const value of set) {
      setValues.push(value);
    }

    expect(mixed).toEqual([1, 2, 3, 4]);
    expect(entries).toEqual([["a", 1], ["b", 2]]);
    expect(setValues).toEqual([10, 20, 30]);
  });

  test("for-await-of supports destructuring", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        const pairs = [[1, "a"], [2, "b"], [3, "c"]];
        let i = 0;
        return {
          next() {
            if (i < pairs.length) {
              const value = pairs[i];
              i = i + 1;
              return Promise.resolve({ value, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    const nums = [];
    const chars = [];

    for await (const [n, c] of asyncIterable) {
      nums.push(n);
      chars.push(c);
    }

    expect(nums).toEqual([1, 2, 3]);
    expect(chars).toEqual(["a", "b", "c"]);
  });

  test("async generator for-await-of async iterator resumes with pattern bindings after body yield", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        const values = [{ x: 1 }, { y: 2 }, { x: 3 }];
        let i = 0;
        return {
          next() {
            if (i < values.length) {
              const value = values[i];
              i = i + 1;
              return Promise.resolve({ value, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    const obj = {
      async *values() {
        const fns = [];
        for await (const item is { x: const x } of asyncIterable) {
          yield x;
          fns.push(() => x);
        }
        return fns.map((fn) => fn());
      },
    };

    const iter = obj.values();
    expect(await iter.next()).toEqual({ value: 1, done: false });
    expect(await iter.next()).toEqual({ value: 3, done: false });
    expect(await iter.next()).toEqual({ value: [1, 3], done: true });
  });

  test("async generator for-await-of sync fallback resumes with pattern bindings after body yield", async () => {
    const obj = {
      async *values() {
        const fns = [];
        for await (const item is { x: const x } of [{ x: 1 }, { y: 2 }, { x: 3 }]) {
          yield x;
          fns.push(() => x);
        }
        return fns.map((fn) => fn());
      },
    };

    const iter = obj.values();
    expect(await iter.next()).toEqual({ value: 1, done: false });
    expect(await iter.next()).toEqual({ value: 3, done: false });
    expect(await iter.next()).toEqual({ value: [1, 3], done: true });
  });

  test("calls async iterator return on break and awaits result", async () => {
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
            log.push("return:start");
            return Promise.resolve().then(() => {
              log.push("return:end");
              return { done: true };
            });
          }
        };
      }
    };

    await (async () => {
      for await (const x of asyncIter) {
        if (x === 2) break;
      }
    })();

    expect(log.join(",")).toBe("next:1,next:2,return:start,return:end");
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
    let returnCalled = 0;
    const asyncIter = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: 1, done: false });
          },
          return() {
            returnCalled++;
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
    expect(returnCalled).toBe(1);
  });

  test("surfaces .return() error on break", async () => {
    let caughtMessage = "";
    const asyncIter = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: 1, done: false });
          },
          return() {
            return Promise.reject(new Error("return-rejected"));
          }
        };
      }
    };

    try {
      await (async () => {
        for await (const x of asyncIter) {
          break;
        }
      })();
    } catch (e) {
      caughtMessage = e.message;
    }

    expect(caughtMessage).toBe("return-rejected");
  });

  test("surfaces .return() error on return", async () => {
    let caughtMessage = "";
    const asyncIter = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: 1, done: false });
          },
          return() {
            return Promise.reject(new Error("return-rejected"));
          }
        };
      }
    };

    try {
      await (async () => {
        for await (const x of asyncIter) {
          return "done";
        }
      })();
    } catch (e) {
      caughtMessage = e.message;
    }

    expect(caughtMessage).toBe("return-rejected");
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

  test("calls return on sync iterator fallback on return", async () => {
    let returnCalled = 0;
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

    const result = await (async () => {
      for await (const x of iterable) {
        return "done";
      }
      return "missed";
    })();

    expect(result).toBe("done");
    expect(returnCalled).toBe(1);
  });
});
