/*---
description: a generator or async function suspended in a decorator expression keeps its class's decoration state while another class is decorated
features: [decorators, generators, async-await, Goccia.gc]
---*/

// Decorators proposal (tc39/ecma262 PR #2417): a class element's decorator
// expressions take [?Yield, ?Await] from the class body, so a generator or
// async function can suspend part way through a class's decoration. The
// class's decoration state has to survive the suspension, a collection, and
// the decoration of another class in the meantime.

const collectGarbage = () => {
  if (typeof Goccia !== "undefined") {
    Goccia.gc();
  }
};

const recordingDecorator = (log, tag) => (value, context) => {
  collectGarbage();
  context.addInitializer(() => {
    collectGarbage();
    log.push(tag + "-init");
  });
};

describe("decoration suspended in a decorator expression", () => {
  test("a generator keeps its class's decoration while another class is decorated", () => {
    const log = [];
    const mark = (tag) => recordingDecorator(log, tag);
    const host = {
      *define() {
        class G { @(yield) m() {} @(mark("g2")) n() {} }
        new G();
        return G;
      },
    };
    const it = host.define();
    it.next();
    class P { @(mark("p")) m() {} }
    new P();
    collectGarbage();
    const result = it.next(mark("g1"));
    expect(result.done).toBe(true);
    expect(log.join(",")).toBe("p-init,g1-init,g2-init");
  });

  test("an async function keeps its class's decoration while another class is decorated", async () => {
    const log = [];
    const mark = (tag) => recordingDecorator(log, tag);
    const define = async () => {
      class Q { @(await Promise.resolve(mark("q1"))) m() {} @(mark("q2")) n() {} }
      new Q();
    };
    const pending = define();
    class R { @(mark("r")) m() {} }
    new R();
    collectGarbage();
    await pending;
    expect(log.join(",")).toBe("r-init,q1-init,q2-init");
  });
});
