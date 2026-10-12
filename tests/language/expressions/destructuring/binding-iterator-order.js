/*---
description: |
  Array binding patterns step the iterator one element at a time, run each
  element's default and nested pattern before stepping the next element, and
  call return() once, after the whole pattern, when the iterator is not done.
  ES2026 §8.6.2 BindingInitialization (BindingPattern : ArrayBindingPattern)
  and §8.6.3 IteratorBindingInitialization.
features: [destructuring, iterators, async-functions]
---*/

const logged = (log, values) => {
  let index = 0;
  return {
    [Symbol.iterator]() { return this; },
    next() {
      log.push("next");
      if (index < values.length) {
        return { value: values[index++], done: false };
      }
      return { value: undefined, done: true };
    },
    return() {
      log.push("return");
      return {};
    },
  };
};

const fallback = (log, value) => {
  log.push("default");
  return value;
};

const boom = (log) => {
  log.push("default-throws");
  throw new Error("boom");
};

describe("array binding pattern iterator order", () => {
  describe("declarations", () => {
    test("let runs the default before closing the iterator", () => {
      const log = [];
      let [a = fallback(log, "d")] = logged(log, [undefined, 2]);
      expect(a).toBe("d");
      expect(log).toEqual(["next", "default", "return"]);
    });

    test("const runs each default during its own step", () => {
      const log = [];
      const [a = fallback(log, 1), b = fallback(log, 2)] =
        logged(log, [undefined, undefined, 3]);
      expect([a, b]).toEqual([1, 2]);
      expect(log).toEqual(["next", "default", "next", "default", "return"]);
    });

    test("a default that is not needed is not evaluated", () => {
      const log = [];
      const [a = fallback(log, 1), b] = logged(log, [5, 6, 7]);
      expect([a, b]).toEqual([5, 6]);
      expect(log).toEqual(["next", "next", "return"]);
    });

    test("a default that reads the same iterator sees the next element", () => {
      const log = [];
      const it = logged(log, [undefined, 2, 3]);
      const [a = it.next().value, b] = it;
      expect([a, b]).toEqual([2, 3]);
      expect(log).toEqual(["next", "next", "next", "return"]);
    });

    test("a later default sees an earlier binding", () => {
      const log = [];
      const [a, b = a * 10] = logged(log, [4]);
      expect([a, b]).toEqual([4, 40]);
      expect(log).toEqual(["next", "next"]);
    });

    test("a throwing default closes the iterator after it throws", () => {
      const log = [];
      expect(() => {
        const [a = boom(log)] = logged(log, [undefined, 2]);
      }).toThrow(Error);
      expect(log).toEqual(["next", "default-throws", "return"]);
    });

    test("a default after the iterator is done does not close it", () => {
      const log = [];
      const [a, b = fallback(log, "d")] = logged(log, [1]);
      expect([a, b]).toEqual([1, "d"]);
      expect(log).toEqual(["next", "next", "default"]);
    });

    test("a throwing default after the iterator is done does not close it", () => {
      const log = [];
      expect(() => {
        const [a, b = boom(log)] = logged(log, [1]);
      }).toThrow(Error);
      expect(log).toEqual(["next", "next", "default-throws"]);
    });

    test("a throwing next() does not close the iterator", () => {
      const log = [];
      const it = {
        [Symbol.iterator]() { return this; },
        next() {
          log.push("next-throws");
          throw new Error("next");
        },
        return() {
          log.push("return");
          return {};
        },
      };
      expect(() => {
        const [a = fallback(log, 1)] = it;
      }).toThrow(Error);
      expect(log).toEqual(["next-throws"]);
    });
  });

  describe("nested patterns", () => {
    test("an object pattern element reads its properties during its step", () => {
      const log = [];
      const source = {
        get x() {
          log.push("get x");
          return 1;
        },
      };
      const [{ x }, y] = logged(log, [source, 2, 3]);
      expect([x, y]).toEqual([1, 2]);
      expect(log).toEqual(["next", "get x", "next", "return"]);
    });

    test("a throwing nested getter closes the iterator after it throws", () => {
      const log = [];
      const source = {
        get x() {
          log.push("get x throws");
          throw new Error("getter");
        },
      };
      expect(() => {
        const [{ x }] = logged(log, [source, 2]);
      }).toThrow(Error);
      expect(log).toEqual(["next", "get x throws", "return"]);
    });

    test("a nested array pattern closes the inner iterator before the outer one steps", () => {
      const log = [];
      const inner = logged(log, ["i1", "i2"]);
      const [[a], b] = logged(log, [inner, "o2", "o3"]);
      expect([a, b]).toEqual(["i1", "o2"]);
      expect(log).toEqual(["next", "next", "return", "next", "return"]);
    });

    test("a nested array pattern with a default runs inside the outer step", () => {
      const log = [];
      const [[a = fallback(log, "d")] = [], b] = logged(log, [undefined, 2, 3]);
      expect([a, b]).toEqual(["d", 2]);
      expect(log).toEqual(["next", "default", "next", "return"]);
    });

    test("an array pattern inside an object pattern steps in order", () => {
      const log = [];
      const { list: [a = fallback(log, "d"), b] } =
        { list: logged(log, [undefined, 2, 3]) };
      expect([a, b]).toEqual(["d", 2]);
      expect(log).toEqual(["next", "default", "next", "return"]);
    });
  });

  describe("holes and rest", () => {
    test("holes step the iterator in order with defaults", () => {
      const log = [];
      const [, a = fallback(log, "d"), , b] = logged(log, [1, undefined, 3, 4, 5]);
      expect([a, b]).toEqual(["d", 4]);
      expect(log).toEqual(["next", "next", "default", "next", "next", "return"]);
    });

    test("rest collects the remainder after earlier defaults run", () => {
      const log = [];
      const [a = fallback(log, "d"), ...rest] = logged(log, [undefined, 2]);
      expect(a).toBe("d");
      expect(rest).toEqual([2]);
      expect(log).toEqual(["next", "default", "next", "next"]);
    });

    test("rest after a default that reads the iterator collects what remains", () => {
      const log = [];
      const it = logged(log, [undefined, 2, 3, 4]);
      const [a = it.next().value, ...rest] = it;
      expect(a).toBe(2);
      expect(rest).toEqual([3, 4]);
      expect(log).toEqual(["next", "next", "next", "next", "next"]);
    });

    test("rest with a nested pattern runs it after draining the iterator", () => {
      const log = [];
      const [a = fallback(log, "d"), ...[b, c = fallback(log, "e")]] =
        logged(log, [undefined, 2]);
      expect([a, b, c]).toEqual(["d", 2, "e"]);
      expect(log).toEqual(["next", "default", "next", "next", "default"]);
    });
  });

  describe("parameters", () => {
    test("an arrow parameter runs the default before closing the iterator", () => {
      const log = [];
      const result = (([a = fallback(log, "d")]) => a)(logged(log, [undefined, 2]));
      expect(result).toBe("d");
      expect(log).toEqual(["next", "default", "return"]);
    });

    test("a pattern parameter after other parameters steps in order", () => {
      const log = [];
      const take = (first, [a = fallback(log, first), b], last = "z") =>
        [first, a, b, last];
      expect(take("f", logged(log, [undefined, 2, 3]))).toEqual(["f", "f", 2, "z"]);
      expect(log).toEqual(["next", "default", "next", "return"]);
    });

    test("a method parameter runs the default before closing the iterator", () => {
      const log = [];
      const holder = {
        take([a = fallback(log, "d"), b]) {
          return [a, b];
        },
      };
      expect(holder.take(logged(log, [undefined, 2, 3]))).toEqual(["d", 2]);
      expect(log).toEqual(["next", "default", "next", "return"]);
    });

    test("a class method parameter closes the iterator after a throwing default", () => {
      const log = [];
      class Holder {
        take([a = boom(log)]) {
          return a;
        }
      }
      expect(() => new Holder().take(logged(log, [undefined, 2]))).toThrow(Error);
      expect(log).toEqual(["next", "default-throws", "return"]);
    });
  });

  describe("catch and for-of heads", () => {
    test("a catch parameter runs the default before closing the iterator", () => {
      const log = [];
      let seen;
      try {
        throw logged(log, [undefined, 2]);
      } catch ([a = fallback(log, "d")]) {
        seen = a;
      }
      expect(seen).toBe("d");
      expect(log).toEqual(["next", "default", "return"]);
    });

    test("a catch parameter closes the iterator after a throwing default", () => {
      const log = [];
      expect(() => {
        try {
          throw logged(log, [undefined, 2]);
        } catch ([a = boom(log)]) {
          log.push("body");
        }
      }).toThrow(Error);
      expect(log).toEqual(["next", "default-throws", "return"]);
    });

    test("a for-of const head runs the default before closing the iterator", () => {
      const log = [];
      const seen = [];
      for (const [a = fallback(log, "d"), b] of [logged(log, [undefined, 2, 3])]) {
        seen.push(a, b);
      }
      expect(seen).toEqual(["d", 2]);
      expect(log).toEqual(["next", "default", "next", "return"]);
    });

    test("a for-of let head closes the element iterator after a throwing default", () => {
      const log = [];
      expect(() => {
        for (let [a = boom(log)] of [logged(log, [undefined, 2])]) {
          log.push("body");
        }
      }).toThrow(Error);
      expect(log).toEqual(["next", "default-throws", "return"]);
    });
  });

  describe("async functions", () => {
    test("an async function runs the default before closing an awaited iterator", async () => {
      const log = [];
      const take = async (source) => {
        const [a = fallback(log, "d"), b] = await source;
        return [a, b];
      };
      expect(await take(Promise.resolve(logged(log, [undefined, 2, 3])))).toEqual(["d", 2]);
      expect(log).toEqual(["next", "default", "next", "return"]);
    });

    test("an async function closes the iterator after a throwing default", async () => {
      const log = [];
      const take = async (it) => {
        const [a = boom(log)] = it;
        return a;
      };
      let error;
      try {
        await take(logged(log, [undefined, 2]));
      } catch (e) {
        error = e;
      }
      expect(error.message).toBe("boom");
      expect(log).toEqual(["next", "default-throws", "return"]);
    });
  });

  describe("plain arrays", () => {
    test("a default that grows the array sees the new element", () => {
      const values = [undefined];
      const [a = (values.push("pushed"), "d"), b] = values;
      expect([a, b]).toEqual(["d", "pushed"]);
    });

    test("identifier-only patterns bind elements, holes and rest", () => {
      const [a, , b, ...rest] = [1, 2, 3, 4, 5];
      expect([a, b, rest]).toEqual([1, 3, [4, 5]]);
    });
  });
});
