/*---
description: A top-level await in a module entry suspends the module (ES2026 §16.2.1.7.3.2 ExecuteModule step 10), so it resumes in the same job as an await in an async function
features: [top-level-await, async-generators, for-of]
---*/

// The export makes this file module source; its top level runs before the
// tests below and records the job each step ran in, one "tN" apart.
export {};

const log = [];
let chain = Promise.resolve();
for (const i of Array.from({ length: 12 }, (_, j) => j + 1)) {
  chain = chain.then(() => {
    log.push("t" + i);
  });
}

const helpers = {
  async *values() {
    try {
      yield 1;
      yield 2;
    } finally {
      log.push("finally");
    }
  },
};

// A promise the await finds pending.
await helpers.values().next();
log.push("next");

// A thenable: its then method runs in a job of its own.
await {
  then(resolve) {
    log.push("then");
    resolve();
  },
};
log.push("thenable");

// A for await close awaits the iterator's return().
for await (const x of helpers.values()) {
  log.push("body");
  break;
}
log.push("after loop");

// An async-from-sync iterator over an array.
for await (const x of [1]) {
  log.push("array body");
  break;
}
log.push("after array loop");

await chain;

describe("top-level await in a module entry", () => {
  test("resumes in the job an async function's await would", () => {
    expect(log).toEqual([
      "t1",
      "t2",
      "next",
      "t3",
      "then",
      "t4",
      "thenable",
      "t5",
      "t6",
      "body",
      "t7",
      "finally",
      "t8",
      "after loop",
      "t9",
      "t10",
      "array body",
      "t11",
      "after array loop",
      "t12",
    ]);
  });
});
