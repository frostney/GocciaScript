/*---
description: a rejection that gets a handler is not an unhandled rejection
features: [Promise, async-functions, async-iteration]
---*/

// The test runner fails a test that leaves a promise rejected with no handler
// once the test has nothing left to run (ES2026 §27.2.1.9
// HostPromiseRejectionTracker). Each test here handles its rejection in a
// different way, so a pass is also the evidence that the handler was seen.

const fail = (message) => Promise.reject(new Error(message));

describe("a handled rejection is not reported", () => {
  test("handler attached from a later job", async () => {
    const rejected = fail("later job");
    let reason;
    queueMicrotask(() => rejected.catch((error) => { reason = error.message; }));
    await null;
    await null;
    expect(reason).toBe("later job");
  });

  test("handler attached after an await", async () => {
    const rejected = fail("after await");
    await null;
    await expect(rejected).rejects.toThrow("after await");
  });

  test("await inside try/catch", async () => {
    let reason;
    try {
      await fail("awaited");
    } catch (error) {
      reason = error.message;
    }
    expect(reason).toBe("awaited");
  });

  test("async function that throws, caught by its caller", async () => {
    const thrower = async () => { throw new TypeError("async throw"); };
    const reason = await thrower().catch((error) => error.name);
    expect(reason).toBe("TypeError");
  });

  test("rejection passed through then and finally to a catch", async () => {
    const steps = [];
    const reason = await fail("chained")
      .then(() => steps.push("then"))
      .finally(() => steps.push("finally"))
      .catch((error) => error.message);
    expect(reason).toBe("chained");
    expect(steps).toEqual(["finally"]);
  });

  test("rejected promise adopted by another promise", async () => {
    const adopted = new Promise((resolve) => resolve(fail("adopted")));
    const reason = await adopted.catch((error) => error.message);
    expect(reason).toBe("adopted");
  });

  test("rejected promise returned from an async function", async () => {
    const forward = async () => fail("forwarded");
    const reason = await forward().catch((error) => error.message);
    expect(reason).toBe("forwarded");
  });

  test("withResolvers rejected before its handler exists", async () => {
    const { promise, reject } = Promise.withResolvers();
    reject(new Error("resolvers"));
    await null;
    const reason = await promise.catch((error) => error.message);
    expect(reason).toBe("resolvers");
  });
});

describe("a handler that arrives from a queued job counts", () => {
  // The body ends with the handler still queued: nothing is awaited, and the
  // returned promise is already settled.
  test("in an async test that does not await it", async () => {
    const rejected = fail("async, not awaited");
    queueMicrotask(() => rejected.catch(() => {}));
  });

  test("in a test that returns a settled promise", () => {
    const rejected = fail("returned settled");
    queueMicrotask(() => rejected.catch(() => {}));
    return Promise.resolve(1);
  });
});

describe("awaiting a thenable that rejects is handled by the await", () => {
  const rejecting = (message) => ({
    then(_resolve, reject) {
      reject(new Error(message));
    },
  });
  const reasonOf = async (run) => {
    try {
      await run();
    } catch (error) {
      return error.message;
    }
    return "not rejected";
  };

  test("await", async () => {
    expect(await reasonOf(async () => { await rejecting("await"); })).toBe("await");
  });

  test("async iterator whose next returns it", async () => {
    const source = { [Symbol.asyncIterator]: () => ({ next: () => rejecting("next") }) };
    expect(await reasonOf(async () => { for await (const value of source) { expect(value).toBe(0); } })).toBe("next");
  });

  test("member of an array iterated with for await", async () => {
    expect(await reasonOf(async () => { for await (const value of [rejecting("member")]) { expect(value).toBe(0); } })).toBe("member");
  });

  test("member passed to Array.fromAsync", async () => {
    expect(await reasonOf(() => Array.fromAsync([rejecting("fromAsync")]))).toBe("fromAsync");
  });

  test("asynchronous disposer", async () => {
    expect(await reasonOf(async () => {
      // In a block: interpreter mode has no `using` directly in a function body.
      {
        await using resource = { [Symbol.asyncDispose]: () => rejecting("dispose") };
      }
    })).toBe("dispose");
  });

  test("delegate of yield*", async () => {
    const delegating = {
      async *outer() {
        yield* { [Symbol.asyncIterator]: () => ({ next: () => rejecting("delegate") }) };
      },
    };
    expect(await reasonOf(async () => { for await (const value of delegating.outer()) { expect(value).toBe(0); } })).toBe("delegate");
  });

  test("rejected promise passed to an async generator's return", async () => {
    const generator = { async *values() { yield 1; } }.values();
    await generator.next();
    expect(await reasonOf(() => generator.return(fail("return argument")))).toBe("return argument");
  });
});

describe("a rejecting member of a combinator is handled by the combinator", () => {
  test("Promise.all", async () => {
    const reason = await Promise.all([fail("all"), Promise.resolve(1), fail("all second")])
      .catch((error) => error.message);
    expect(reason).toBe("all");
  });

  test("Promise.allSettled", async () => {
    const results = await Promise.allSettled([fail("settled"), Promise.resolve(1)]);
    expect(results.map((result) => result.status)).toEqual(["rejected", "fulfilled"]);
  });

  test("Promise.race", async () => {
    const reason = await Promise.race([fail("race"), fail("race loser")])
      .catch((error) => error.message);
    expect(reason).toBe("race");
  });

  test("Promise.any with a fulfilled member", async () => {
    const value = await Promise.any([fail("any"), Promise.resolve("won")]);
    expect(value).toBe("won");
  });

  test("Promise.any with only rejections", async () => {
    const name = await Promise.any([fail("any one"), fail("any two")])
      .catch((error) => error.name);
    expect(name).toBe("AggregateError");
  });
});

describe("a rejection consumed by iteration is handled", () => {
  test("for await over an async generator that throws", async () => {
    const source = {
      async *values() {
        yield 1;
        throw new Error("generator");
      },
    };
    const seen = [];
    let reason;
    try {
      for await (const value of source.values()) {
        seen.push(value);
      }
    } catch (error) {
      reason = error.message;
    }
    expect(seen).toEqual([1]);
    expect(reason).toBe("generator");
  });

  test("for await over an array holding a rejected promise", async () => {
    let reason;
    try {
      for await (const value of [Promise.resolve(1), fail("in array")]) {
        expect(value).toBe(1);
      }
    } catch (error) {
      reason = error.message;
    }
    expect(reason).toBe("in array");
  });

  test("Array.fromAsync rejecting on a member", async () => {
    const reason = await Array.fromAsync([Promise.resolve(1), fail("fromAsync")])
      .catch((error) => error.message);
    expect(reason).toBe("fromAsync");
  });
});
