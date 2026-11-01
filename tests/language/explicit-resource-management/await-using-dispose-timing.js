/*---
description: An await using disposal follows ES2027 §7.5.4 DisposeResources, which awaits a dispose result once, does not await after a dispose call throws, and awaits undefined once for null resources
features: [explicit-resource-management, async-await]
---*/

// Logs "t1", "t2", ... one job apart, so a log records the job each step ran in.
const ticker = (log, count) => {
  let chain = Promise.resolve();
  for (const i of Array.from({ length: count }, (_, j) => j + 1)) {
    chain = chain.then(() => {
      log.push("t" + i);
    });
  }
  return chain;
};

const describeError = (e) => {
  if (e instanceof SuppressedError) {
    return (
      "Suppressed(" +
      describeError(e.error) +
      ", " +
      describeError(e.suppressed) +
      ")"
    );
  }
  return e instanceof Error ? e.message : String(e);
};

// Starts a ticker, then runs body(log) as an async function, and resolves to
// its log and outcome once both are done.
const observe = async (body, count = 6) => {
  const log = [];
  const ticks = ticker(log, count);
  let outcome = "fulfilled";
  const run = body(log).then(
    () => {
      log.push("settled");
    },
    (e) => {
      outcome = describeError(e);
      log.push("settled");
    }
  );
  await Promise.all([run, ticks]);
  return { log, outcome };
};

const asyncResource = (name, log, result) => ({
  [Symbol.asyncDispose]() {
    log.push("dispose " + name);
    return result();
  },
});

describe("await using disposal timing", () => {
  test("an asyncDispose method's result is awaited once", async () => {
    const { log } = await observe(async (log) => {
      await using r = asyncResource("r", log, () => undefined);
      log.push("body");
    });
    expect(log).toEqual([
      "body",
      "dispose r",
      "t1",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("an asyncDispose method's fulfilled promise is awaited once", async () => {
    const { log } = await observe(async (log) => {
      await using r = asyncResource("r", log, () => Promise.resolve());
      log.push("body");
    });
    expect(log).toEqual([
      "body",
      "dispose r",
      "t1",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("an asyncDispose method that throws is not awaited", async () => {
    const { log, outcome } = await observe(async (log) => {
      await using r = asyncResource("r", log, () => {
        throw new Error("thrown");
      });
    });
    expect(outcome).toBe("thrown");
    expect(log).toEqual([
      "dispose r",
      "t1",
      "settled",
      "t2",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("an asyncDispose method's rejection is awaited once", async () => {
    const { log, outcome } = await observe(async (log) => {
      await using r = asyncResource("r", log, () =>
        Promise.reject(new Error("rejected"))
      );
    });
    expect(outcome).toBe("rejected");
    expect(log).toEqual([
      "dispose r",
      "t1",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("a thenable result is resolved through its then method", async () => {
    const { log } = await observe(async (log) => {
      await using r = asyncResource("r", log, () => ({
        then(resolve) {
          log.push("then");
          resolve();
        },
      }));
    });
    expect(log).toEqual([
      "dispose r",
      "t1",
      "then",
      "t2",
      "t3",
      "settled",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("a dispose method's promise is not settled by draining queued jobs", async () => {
    const { log } = await observe(async (log) => {
      await using r = asyncResource(
        "r",
        log,
        () =>
          new Promise((resolve) => {
            Promise.resolve()
              .then(() => {})
              .then(() => {
                log.push("resolve");
                resolve();
              });
          })
      );
    });
    expect(log).toEqual([
      "dispose r",
      "t1",
      "t2",
      "resolve",
      "t3",
      "t4",
      "settled",
      "t5",
      "t6",
    ]);
  });

  test("a Symbol.dispose method is awaited as a promise for undefined", async () => {
    const { log } = await observe(async (log) => {
      await using r = {
        [Symbol.dispose]() {
          log.push("dispose r");
          const rejected = Promise.reject(new Error("never awaited"));
          rejected.catch(() => {});
          return rejected;
        },
      };
    });
    expect(log).toEqual([
      "dispose r",
      "t1",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("a Symbol.dispose method's throw is awaited as a rejection", async () => {
    const { log, outcome } = await observe(async (log) => {
      await using r = {
        [Symbol.dispose]() {
          log.push("dispose r");
          throw new Error("thrown");
        },
      };
    });
    expect(outcome).toBe("thrown");
    expect(log).toEqual([
      "dispose r",
      "t1",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("null resources await undefined once", async () => {
    const { log } = await observe(async (log) => {
      {
        await using a = null,
          b = undefined;
        log.push("body");
      }
      log.push("after");
    });
    expect(log).toEqual([
      "body",
      "t1",
      "after",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("a null resource adds no await after a dispose result was awaited", async () => {
    const { log } = await observe(async (log) => {
      {
        await using a = null,
          b = asyncResource("b", log, () => undefined);
      }
      log.push("after");
    });
    expect(log).toEqual([
      "dispose b",
      "t1",
      "after",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("a null resource's await runs before the next using resource is disposed", async () => {
    const { log } = await observe(async (log) => {
      using a = {
        [Symbol.dispose]() {
          log.push("dispose a");
        },
      };
      await using b = null;
      log.push("body");
    });
    expect(log).toEqual([
      "body",
      "t1",
      "dispose a",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("using and await using resources dispose in reverse order", async () => {
    const { log } = await observe(async (log) => {
      using a = {
        [Symbol.dispose]() {
          log.push("dispose a");
        },
      };
      await using b = null;
      await using c = asyncResource("c", log, () => undefined);
      using d = {
        [Symbol.dispose]() {
          log.push("dispose d");
        },
      };
    });
    expect(log).toEqual([
      "dispose d",
      "dispose c",
      "t1",
      "dispose a",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("errors from dispose calls and awaits are suppressed in order", async () => {
    const rejecting = (name, log) =>
      asyncResource(name, log, () => Promise.reject(new Error(name)));
    const results = [];
    results.push(
      await observe(async (log) => {
        await using a = rejecting("a", log);
        throw new Error("body");
      })
    );
    results.push(
      await observe(async (log) => {
        await using a = rejecting("a", log),
          b = rejecting("b", log);
      })
    );
    results.push(
      await observe(async (log) => {
        await using a = asyncResource("a", log, () => {
            throw new Error("a");
          }),
          b = rejecting("b", log);
      })
    );
    expect(results).toEqual([
      {
        outcome: "Suppressed(a, body)",
        log: ["dispose a", "t1", "t2", "settled", "t3", "t4", "t5", "t6"],
      },
      {
        outcome: "Suppressed(a, b)",
        log: [
          "dispose b",
          "t1",
          "dispose a",
          "t2",
          "t3",
          "settled",
          "t4",
          "t5",
          "t6",
        ],
      },
      {
        outcome: "Suppressed(a, b)",
        log: [
          "dispose b",
          "t1",
          "dispose a",
          "t2",
          "settled",
          "t3",
          "t4",
          "t5",
          "t6",
        ],
      },
    ]);
  });

  test("break and return dispose with the same awaits", async () => {
    const broken = await observe(async (log) => {
      for (const i of [1, 2]) {
        await using r = asyncResource("r" + i, log, () => undefined);
        if (i === 1) {
          break;
        }
      }
      log.push("after loop");
    });
    expect(broken.log).toEqual([
      "dispose r1",
      "t1",
      "after loop",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);

    const returned = await observe(async (log) => {
      {
        await using r = asyncResource("r", log, () =>
          Promise.reject(new Error("rejected"))
        );
        return "value";
      }
    });
    expect(returned.outcome).toBe("rejected");
    expect(returned.log).toEqual([
      "dispose r",
      "t1",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });

  test("a for-of await using head disposes each value with the same awaits", async () => {
    const { log, outcome } = await observe(async (log) => {
      for (await using x of [
        asyncResource("x1", log, () => undefined),
        asyncResource("x2", log, () => {
          throw new Error("x2");
        }),
      ]) {
        log.push("body");
      }
    });
    expect(outcome).toBe("x2");
    expect(log).toEqual([
      "body",
      "dispose x1",
      "t1",
      "body",
      "dispose x2",
      "t2",
      "settled",
      "t3",
      "t4",
      "t5",
      "t6",
    ]);
  });
});
