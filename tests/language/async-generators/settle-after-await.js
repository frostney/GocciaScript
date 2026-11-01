/*---
description: After an async generator body resumes from an await, a returned or yielded value is awaited by a reaction job before the request settles (ES2026 §14.10.1 step 3, §27.5.3.7 Yield, §27.6.3.2 AsyncGeneratorStart)
features: [async-generators]
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

const makeThenable = (log, value) => ({
  then(resolve) {
    log.push("then");
    resolve(value);
  },
});

describe("async generator settling after an await", () => {
  test("a returned thenable settles next() from the Await's reaction", async () => {
    const log = [];
    const helpers = {
      async *values(value) {
        await null;
        return value;
      },
    };
    const request = helpers
      .values(makeThenable(log, 42))
      .next()
      .then((r) => log.push("next:" + r.value + ":" + r.done));
    await Promise.all([request, ticker(log, 4)]);
    expect(log).toEqual([
      "t1",
      "then",
      "t2",
      "t3",
      "next:42:true",
      "t4",
    ]);
  });

  test("a thenable returned by a finally that return() runs settles from the Await's reaction", async () => {
    const log = [];
    const thenable = makeThenable(log, 42);
    const helpers = {
      async *values() {
        try {
          yield 1;
        } finally {
          return thenable;
        }
      },
    };
    const iterator = helpers.values();
    await iterator.next();
    log.push("first");
    const request = iterator
      .return(0)
      .then((r) => log.push("return:" + r.value + ":" + r.done));
    await Promise.all([request, ticker(log, 4)]);
    expect(log).toEqual([
      "first",
      "t1",
      "then",
      "t2",
      "t3",
      "return:42:true",
      "t4",
    ]);
  });

  test("a yielded thenable settles next() from the Await's reaction", async () => {
    const log = [];
    const thenable = makeThenable(log, 42);
    const helpers = {
      async *values() {
        await null;
        yield thenable;
      },
    };
    const request = helpers
      .values()
      .next()
      .then((r) => log.push("next:" + r.value + ":" + r.done));
    await Promise.all([request, ticker(log, 4)]);
    expect(log).toEqual([
      "t1",
      "then",
      "t2",
      "t3",
      "next:42:false",
      "t4",
    ]);
  });

  test("a thenable yielded by a finally that return() runs is awaited", async () => {
    const log = [];
    const thenable = makeThenable(log, 42);
    const helpers = {
      async *values() {
        try {
          yield 1;
        } finally {
          yield thenable;
        }
      },
    };
    const iterator = helpers.values();
    await iterator.next();
    log.push("first");
    const request = iterator
      .return(0)
      .then((r) => log.push("return:" + r.value + ":" + r.done));
    await Promise.all([request, ticker(log, 4)]);
    expect(log).toEqual([
      "first",
      "t1",
      "then",
      "t2",
      "t3",
      "return:42:false",
      "t4",
    ]);
  });

  test("requests queued behind a returned thenable settle in order after it", async () => {
    const log = [];
    const helpers = {
      async *values(value) {
        await null;
        return value;
      },
    };
    const iterator = helpers.values(makeThenable(log, 42));
    const requests = [
      iterator.next().then((r) => log.push("n1:" + r.value)),
      iterator.next().then((r) => log.push("n2:" + r.done)),
      iterator.return(7).then((r) => log.push("r:" + r.value)),
    ];
    await Promise.all([...requests, ticker(log, 5)]);
    expect(log).toEqual([
      "t1",
      "then",
      "t2",
      "t3",
      "n1:42",
      "n2:true",
      "t4",
      "r:7",
      "t5",
    ]);
  });

  test("a rejected returned value rejects next() and completes the generator", async () => {
    const log = [];
    const helpers = {
      async *values(value) {
        await null;
        return value;
      },
    };
    const iterator = helpers.values(Promise.reject("x"));
    const requests = [
      iterator.next().then(
        () => log.push("fulfilled"),
        (e) => log.push("rejected:" + e)
      ),
      iterator.next().then((r) => log.push("n2:" + r.done)),
    ];
    await Promise.all([...requests, ticker(log, 3)]);
    expect(log).toEqual(["t1", "t2", "rejected:x", "n2:true", "t3"]);
  });

  test("a rejected yielded value throws at the yield", async () => {
    const log = [];
    const helpers = {
      async *values() {
        await null;
        try {
          yield Promise.reject("bad");
        } catch (e) {
          log.push("caught:" + e);
          yield "after";
        }
      },
    };
    const iterator = helpers.values();
    const requests = [
      iterator.next().then((r) => log.push("n1:" + r.value)),
      iterator.next().then((r) => log.push("n2:" + r.done)),
    ];
    await Promise.all([...requests, ticker(log, 4)]);
    expect(log).toEqual([
      "t1",
      "caught:bad",
      "t2",
      "t3",
      "n1:after",
      "n2:true",
      "t4",
    ]);
  });

  test("a plain returned value and a return that leaves a try settle in order with other jobs", async () => {
    const log = [];
    const helpers = {
      async *plain() {
        await null;
        return 1;
      },
      async *inTry(value) {
        await null;
        try {
          return value;
        } finally {
          log.push("finally");
        }
      },
    };
    const requests = [
      helpers
        .plain()
        .next()
        .then((r) => log.push("plain:" + r.value)),
      helpers
        .inTry(makeThenable(log, 42))
        .next()
        .then((r) => log.push("inTry:" + r.value)),
    ];
    await Promise.all([...requests, ticker(log, 4)]);
    expect(log).toEqual([
      "t1",
      "then",
      "t2",
      "plain:1",
      "finally",
      "t3",
      "inTry:42",
      "t4",
    ]);
  });
});
