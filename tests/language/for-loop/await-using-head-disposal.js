/*---
description: A for statement whose head is an await using declaration disposes only the bindings whose initializers ran (ES2027 §7.5.2 AddDisposableResource, §7.5.4 DisposeResources)
features: [explicit-resource-management, async-await]
---*/

const throwing = () => {
  throw new Error("initializer");
};

const resource = (name, log) => ({
  [Symbol.asyncDispose]() {
    log.push("dispose " + name);
    // A disposal must never call what an earlier disposal returned.
    return () => {
      log.push("called " + name);
    };
  },
});

describe("for statement with an await using head", () => {
  test("an initializer that throws skips the bindings after it", async () => {
    const log = [];
    for (const i of [1, 2]) {
      try {
        for (
          await using a = i === 2 ? throwing() : resource("a", log),
            b = resource("b", log);
          ;
        ) {
          break;
        }
      } catch (e) {
        log.push("caught " + e.message);
      }
    }
    expect(log).toEqual(["dispose b", "dispose a", "caught initializer"]);
  });

  test("a first initializer that throws disposes nothing and awaits nothing", async () => {
    const log = [];
    let chain = Promise.resolve();
    for (const i of [1, 2, 3]) {
      chain = chain.then(() => {
        log.push("t" + i);
      });
    }
    const loop = (async () => {
      for (await using a = throwing(), b = resource("b", log); ;) {
        break;
      }
    })().catch((e) => {
      log.push("caught " + e.message);
    });
    await Promise.all([loop, chain]);
    expect(log).toEqual(["t1", "caught initializer", "t2", "t3"]);
  });
});
