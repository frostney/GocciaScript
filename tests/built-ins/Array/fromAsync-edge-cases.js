/*---
description: >
  Edge cases for Array.fromAsync per ES2026 §23.1.2.1 and test262
  test/built-ins/Array/fromAsync/
features: [Array.fromAsync, async-await]
---*/

describe("Array.fromAsync basic properties", () => {
  test("returns a Promise", () => {
    const result = Array.fromAsync([]);
    expect(result instanceof Promise).toBe(true);
  });

  test("no arguments rejects with TypeError", () => {
    return Array.fromAsync().then(
      () => {
        expect(true).toBe(false);
      },
      (err) => {
        expect(err.name).toBe("TypeError");
      }
    );
  });
});

describe("Array.fromAsync with sync iterables", () => {
  test("array of numbers", () => {
    return Array.fromAsync([1, 2, 3]).then((result) => {
      expect(result).toEqual([1, 2, 3]);
    });
  });

  test("empty array", () => {
    return Array.fromAsync([]).then((result) => {
      expect(result).toEqual([]);
    });
  });

  test("single element array", () => {
    return Array.fromAsync([42]).then((result) => {
      expect(result).toEqual([42]);
    });
  });

  test("array with null and undefined elements", () => {
    return Array.fromAsync([null, undefined, null]).then((result) => {
      expect(result).toHaveLength(3);
      expect(result[0]).toBeNull();
      expect(result[1]).toBeUndefined();
      expect(result[2]).toBeNull();
    });
  });

  test("array with mixed types", () => {
    return Array.fromAsync([1, "two", true, null]).then((result) => {
      expect(result).toEqual([1, "two", true, null]);
    });
  });

  test("Set as sync iterable", () => {
    return Array.fromAsync(new Set([10, 20, 30])).then((result) => {
      expect(result).toEqual([10, 20, 30]);
    });
  });

  test("Map as sync iterable", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);
    return Array.fromAsync(map).then((result) => {
      expect(result).toEqual([["a", 1], ["b", 2]]);
    });
  });

  test("string as sync iterable", () => {
    return Array.fromAsync("abc").then((result) => {
      expect(result).toEqual(["a", "b", "c"]);
    });
  });

  test("empty string", () => {
    return Array.fromAsync("").then((result) => {
      expect(result).toEqual([]);
    });
  });
});

describe("Array.fromAsync awaits Promise elements from sync iterable", () => {
  test("resolves all Promise elements", () => {
    return Array.fromAsync([
      Promise.resolve("a"),
      Promise.resolve("b"),
      Promise.resolve("c")
    ]).then((result) => {
      expect(result).toEqual(["a", "b", "c"]);
    });
  });

  test("mix of Promise and non-Promise elements", () => {
    return Array.fromAsync([
      1,
      Promise.resolve(2),
      3,
      Promise.resolve(4)
    ]).then((result) => {
      expect(result).toEqual([1, 2, 3, 4]);
    });
  });

  test("Promise resolving to null", () => {
    return Array.fromAsync([Promise.resolve(null)]).then((result) => {
      expect(result[0]).toBeNull();
    });
  });

  test("Promise resolving to undefined", () => {
    return Array.fromAsync([Promise.resolve(undefined)]).then((result) => {
      expect(result[0]).toBeUndefined();
    });
  });
});

describe("Array.fromAsync with async iterables", () => {
  test("basic async iterable", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return Promise.resolve({ value: i * 10, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    return Array.fromAsync(asyncIterable).then((result) => {
      expect(result).toEqual([10, 20, 30]);
    });
  });

  test("empty async iterable", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    return Array.fromAsync(asyncIterable).then((result) => {
      expect(result).toEqual([]);
    });
  });

  test("async iterable with non-Promise next()", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 2) {
              return { value: i, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    return Array.fromAsync(asyncIterable).then((result) => {
      expect(result).toEqual([1, 2]);
    });
  });

  test("Symbol.asyncIterator takes priority over Symbol.iterator", () => {
    const obj = {
      [Symbol.asyncIterator]() {
        return {
          count: 0,
          next() {
            this.count = this.count + 1;
            if (this.count <= 1) {
              return Promise.resolve({ value: "async", done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      },
      [Symbol.iterator]() {
        return {
          next() {
            return { value: "sync", done: true };
          }
        };
      }
    };
    return Array.fromAsync(obj).then((result) => {
      expect(result[0]).toBe("async");
    });
  });
});

describe("Array.fromAsync with mapping function", () => {
  test("sync mapfn with sync iterable", () => {
    return Array.fromAsync([1, 2, 3], (x) => x * 2).then((result) => {
      expect(result).toEqual([2, 4, 6]);
    });
  });

  test("mapfn receives element and index", () => {
    const indices = [];
    return Array.fromAsync([10, 20, 30], (val, idx) => {
      indices.push(idx);
      return val;
    }).then((result) => {
      expect(result).toEqual([10, 20, 30]);
      expect(indices).toEqual([0, 1, 2]);
    });
  });

  test("async mapfn with sync iterable", () => {
    return Array.fromAsync([1, 2, 3], async (x) => {
      const doubled = await Promise.resolve(x * 2);
      return doubled;
    }).then((result) => {
      expect(result).toEqual([2, 4, 6]);
    });
  });

  test("mapfn with Promise elements (elements awaited before mapping)", () => {
    return Array.fromAsync(
      [Promise.resolve(1), Promise.resolve(2), Promise.resolve(3)],
      (x) => x * 10
    ).then((result) => {
      expect(result).toEqual([10, 20, 30]);
    });
  });

  test("mapfn returning null/undefined", () => {
    return Array.fromAsync([1, 2], () => null).then((result) => {
      expect(result[0]).toBeNull();
      expect(result[1]).toBeNull();
    });
  });

  test("non-callable mapfn rejects the Promise", () => {
    return Array.fromAsync([1, 2, 3], "not a function").then(
      () => {
        expect(true).toBe(false);
      },
      (err) => {
        expect(err.name).toBe("TypeError");
      }
    );
  });
});

describe("Array.fromAsync with async iterable and mapping", () => {
  test("sync mapfn with async iterable", () => {
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
    return Array.fromAsync(asyncIterable, (x) => x * 100).then((result) => {
      expect(result).toEqual([100, 200, 300]);
    });
  });
});

describe("Array.fromAsync with custom sync iterables", () => {
  test("custom iterable object", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return { value: i * 5, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    return Array.fromAsync(iterable).then((result) => {
      expect(result).toEqual([5, 10, 15]);
    });
  });

  test("Set.values() as iterable", () => {
    const set = new Set([100, 200, 300]);
    return Array.fromAsync(set.values()).then((result) => {
      expect(result).toEqual([100, 200, 300]);
    });
  });
});

describe("Array.fromAsync error handling", () => {
  test("rejected Promise element rejects the result", () => {
    return Array.fromAsync([
      Promise.resolve(1),
      Promise.reject("element error"),
      Promise.resolve(3)
    ]).then(
      () => {
        expect(true).toBe(false);
      },
      (err) => {
        expect(err).toBe("element error");
      }
    );
  });

  test("async iterable with rejected next() rejects the result", () => {
    const asyncIterable = {
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
    return Array.fromAsync(asyncIterable).then(
      () => {
        expect(true).toBe(false);
      },
      (err) => {
        expect(err).toBe("iteration error");
      }
    );
  });
});

describe("Array.fromAsync with null/undefined input", () => {
  // test262: asyncitems-null-undefined.js
  test("null input rejects", () => {
    return Array.fromAsync(null).then(
      () => {
        expect(true).toBe(false);
      },
      (err) => {
        expect(err.name).toBe("TypeError");
      }
    );
  });

  test("undefined input rejects", () => {
    return Array.fromAsync(undefined).then(
      () => {
        expect(true).toBe(false);
      },
      (err) => {
        expect(err.name).toBe("TypeError");
      }
    );
  });
});
