/*---
description: a top-level-await module's execution-context entry survives collections across every resumption
features: [Goccia.gc, async-await, top-level-await]
---*/

// A module body containing top-level await is evaluated through
// TGocciaInterpreterAsyncModuleEvaluation, whose Resume pushes an execution
// context naming the *module* scope — the one place an execution-context entry
// carries something other than the engine global scope or a VM frame's
// closure. Each resumption re-pushes that entry, and between resumptions the
// only thing referring to the evaluation is the await reaction, so the module
// scope has to stay marked for the entry to stay valid. Collecting twice at
// every suspension point and then reading module-scope bindings back is what
// catches a scope that was swept and recycled.
const collect = () => {
  Goccia.gc();
  Goccia.gc();
};

collect();

const beforeFirstAwait = "before";
const first = await Promise.resolve("first");
collect();

const second = await Promise.resolve(first + "-second");
collect();

// A binding written after a resumption, read after two further ones.
let accumulated = beforeFirstAwait;
accumulated += "|" + second;

const third = await (async () => {
  collect();
  await Promise.resolve();
  collect();
  return "third";
})();
collect();

accumulated += "|" + third;

let caught = null;
try {
  await Promise.reject(new Error("rejected"));
} catch (e) {
  caught = e;
}
collect();

// A binding captured by a closure created before the last suspension, invoked
// after it: the closure resolves through the module scope the entry names.
const readAccumulated = () => accumulated;
await Promise.resolve();
collect();

describe("top-level-await module execution context", () => {
  test("bindings written before the first await survive every collection", () => {
    expect(beforeFirstAwait).toBe("before");
    collect();
    expect(beforeFirstAwait).toBe("before");
  });

  test("bindings written between resumptions survive", () => {
    collect();
    expect(first).toBe("first");
    expect(second).toBe("first-second");
    expect(third).toBe("third");
  });

  test("a rejection resumption leaves the module scope intact", () => {
    collect();
    expect(caught.message).toBe("rejected");
  });

  test("a closure over the module scope still resolves after collections", () => {
    collect();
    expect(readAccumulated()).toBe("before|first-second|third");
    collect();
    expect(readAccumulated()).toBe("before|first-second|third");
  });
});
