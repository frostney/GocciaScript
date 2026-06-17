/*---
description: Bytecode async-await continuation scheduling regression tests
features: [async-await]
---*/

const __bytecodeMode =
  typeof __gocciaTestRunnerMode !== "undefined" &&
  __gocciaTestRunnerMode === "bytecode";

test.runIf(__bytecodeMode)(
  "await on thenables resumes after already queued promise reactions",
  async () => {
    const actual = [];
    let thenCallCount = 0;
    const thenable = {
      then(resolve) {
        thenCallCount++;
        resolve(thenCallCount);
      },
    };
    const helpers = {
      async trigger() {
        actual.push("await " + await thenable);
        actual.push("await " + await thenable);
      },
    };

    const triggerDone = helpers.trigger();
    const promiseDone = new Promise((resolve) => {
      actual.push("promise 1");
      resolve();
    })
      .then(() => actual.push("promise 2"))
      .then(() => actual.push("promise 3"))
      .then(() => actual.push("promise 4"));

    await Promise.all([triggerDone, promiseDone]);

    expect(actual).toEqual([
      "promise 1",
      "promise 2",
      "await 1",
      "promise 3",
      "promise 4",
      "await 2",
    ]);
    expect(thenCallCount).toBe(2);
  }
);
