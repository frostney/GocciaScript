/*---
description: instanceof keeps the walked prototype reachable across a proxy getPrototypeOf trap
features: [Goccia.gc, Proxy, Reflect]
---*/

const hasGoccia = typeof Goccia !== "undefined";

const range = (n) => Array.from({ length: n }, (_, i) => i);

// A bare gc() usually leaves a freed slot readable; allocating a run of objects
// of the same GC class as the collected temporary is what overwrites the slot
// and makes a dangling read observable. The walked prototype is a Proxy, so the
// churn allocates proxies.
const churn = () => {
  Goccia.gc();
  Goccia.gc();
  let sink = null;
  for (const i of range(200)) {
    sink = new Proxy({ a: i, b: i + 1 }, { get(t, k) { return t[k]; } });
  }
  return sink;
};

describe.runIf(hasGoccia)("instanceof prototype-chain GC roots", () => {
  // OrdinaryHasInstance walks the instance's prototype chain with
  // GetPrototypeOfObject, which invokes a proxy getPrototypeOf trap — guest code
  // that can force a collection. An intermediate prototype reachable only through
  // the walk (here, a fresh proxy returned by an outer trap) is held solely in
  // the loop variable; DispatchProxyGetPrototype dereferences the proxy's
  // internal target for its post-trap invariant check, so without a root the
  // proxy is swept mid-walk and the invariant check reads freed memory.
  test("an engine-only intermediate proxy survives its getPrototypeOf trap", () => {
    let outerRuns = 0;
    let innerRuns = 0;
    class C {}
    const obj = {};

    Reflect.setPrototypeOf(
      obj,
      new Proxy(
        {},
        {
          getPrototypeOf() {
            outerRuns += 1;
            const innerTarget = Object.preventExtensions(Object.create(null));
            return new Proxy(innerTarget, {
              getPrototypeOf(t) {
                innerRuns += 1;
                churn();
                return Reflect.getPrototypeOf(t);
              },
            });
          },
        }
      )
    );

    const result = obj instanceof C;

    expect(outerRuns).toBe(1);
    expect(innerRuns).toBe(1);
    expect(result).toBe(false);
  });

  // The direct case: the walked proxy is orphaned by the trap that runs on it and
  // must survive to the post-trap extensibility invariant check.
  test("a directly walked proxy survives a collecting getPrototypeOf trap", () => {
    let trapRuns = 0;
    class C {}
    const obj = {};
    const target = Object.preventExtensions(Object.create(null));

    Reflect.setPrototypeOf(
      obj,
      new Proxy(target, {
        getPrototypeOf(t) {
          trapRuns += 1;
          churn();
          return Reflect.getPrototypeOf(t);
        },
      })
    );

    expect(obj instanceof C).toBe(false);
    expect(trapRuns).toBe(1);
  });
});
