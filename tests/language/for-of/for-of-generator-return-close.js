/*---
description: A generator's return() resumes it at a yield inside for...of with a return completion, so ES2026 §7.4.11 IteratorClose propagates errors from the loop iterator's return method and checks its result
features: [for-of, generators]
---*/

const makeSource = (descriptor, log) =>
  Object.defineProperty(
    {
      [Symbol.iterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
    },
    "return",
    {
      get() {
        log.push("get return");
        return descriptor();
      },
    }
  );

const closeThroughLoop = (source) => {
  const finallyLog = [];
  const helpers = {
    *values() {
      try {
        for (const x of source) {
          yield x;
        }
      } finally {
        finallyLog.push("finally");
      }
    },
  };
  const iterator = helpers.values();
  iterator.next();
  let outcome;
  try {
    outcome = iterator.return(9);
  } catch (e) {
    outcome = e instanceof TypeError ? "TypeError" : e;
  }
  return { outcome, finallyLog, after: iterator.next() };
};

describe("generator return() through for...of", () => {
  test("a primitive result from return is a TypeError", () => {
    const log = [];
    const { outcome, finallyLog, after } = closeThroughLoop(
      makeSource(() => () => 0, log)
    );
    expect(outcome).toBe("TypeError");
    expect(log).toEqual(["get return"]);
    expect(finallyLog).toEqual(["finally"]);
    expect(after).toEqual({ value: undefined, done: true });
  });

  test("an undefined result from return is a TypeError", () => {
    const { outcome } = closeThroughLoop(makeSource(() => () => undefined, []));
    expect(outcome).toBe("TypeError");
  });

  test("an error thrown by return propagates", () => {
    const { outcome, finallyLog, after } = closeThroughLoop(
      makeSource(
        () => () => {
          throw "thrown";
        },
        []
      )
    );
    expect(outcome).toBe("thrown");
    expect(finallyLog).toEqual(["finally"]);
    expect(after).toEqual({ value: undefined, done: true });
  });

  test("an error thrown by the return getter propagates", () => {
    const { outcome } = closeThroughLoop(
      makeSource(() => {
        throw "getter";
      }, [])
    );
    expect(outcome).toBe("getter");
  });

  test("a return that is not callable is a TypeError", () => {
    const { outcome } = closeThroughLoop(makeSource(() => 5, []));
    expect(outcome).toBe("TypeError");
  });

  test("an Object result completes the return", () => {
    const log = [];
    const { outcome, finallyLog } = closeThroughLoop(
      makeSource(
        () => () => {
          log.push("return called");
          return {};
        },
        log
      )
    );
    expect(outcome).toEqual({ value: 9, done: true });
    expect(log).toEqual(["get return", "return called"]);
    expect(finallyLog).toEqual(["finally"]);
  });

  test("a null or missing return completes the return", () => {
    expect(closeThroughLoop(makeSource(() => null, [])).outcome).toEqual({
      value: 9,
      done: true,
    });
    expect(closeThroughLoop(makeSource(() => undefined, [])).outcome).toEqual({
      value: 9,
      done: true,
    });
  });

  test("an error from closing a generator source propagates", () => {
    const helpers = {
      *inner() {
        try {
          yield 1;
        } finally {
          throw "inner finally";
        }
      },
    };
    const { outcome } = closeThroughLoop(helpers.inner());
    expect(outcome).toBe("inner finally");
  });

  test("nested loops close inner first and the inner error propagates", () => {
    const log = [];
    const make = (name) => ({
      [Symbol.iterator]() {
        return this;
      },
      next() {
        return { value: 1, done: false };
      },
      return() {
        log.push(name);
        if (name === "inner") {
          throw "inner close";
        }
        return {};
      },
    });
    const helpers = {
      *values() {
        for (const a of make("outer")) {
          for (const b of make("inner")) {
            yield b;
          }
        }
      },
    };
    const iterator = helpers.values();
    iterator.next();
    let outcome;
    try {
      outcome = iterator.return(9);
    } catch (e) {
      outcome = e;
    }
    expect(outcome).toBe("inner close");
    expect(log).toEqual(["inner", "outer"]);
  });

  test("the generator can catch a close error", () => {
    const helpers = {
      *values() {
        try {
          for (const x of makeSource(() => () => 0, [])) {
            yield x;
          }
        } catch (e) {
          yield e instanceof TypeError ? "caught TypeError" : e;
        }
      },
    };
    const iterator = helpers.values();
    iterator.next();
    expect(iterator.return(9)).toEqual({
      value: "caught TypeError",
      done: false,
    });
  });

  test("a throw from the generator still wins over close errors", () => {
    const helpers = {
      *values() {
        for (const x of makeSource(
          () => () => {
            throw "close";
          },
          []
        )) {
          yield x;
        }
      },
    };
    const iterator = helpers.values();
    iterator.next();
    let outcome;
    try {
      iterator.throw("thrown");
    } catch (e) {
      outcome = e;
    }
    expect(outcome).toBe("thrown");
  });
});
