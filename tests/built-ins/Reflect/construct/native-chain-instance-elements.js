/*---
description: Every route through the abstract Construct operation initializes the instance elements of a class whose superclass chain reaches a built-in
features: [Reflect]
---*/

/*
  ES2026 §7.3.33 InitializeInstanceElements runs for every [[Construct]], not
  only for the `new` operator, and §15.7.14 step 15a gives a class with no
  constructor of its own an implicit one that forwards to super and then
  initializes its own elements. So Reflect.construct (§28.1.2), the proxy
  [[Construct]] fallback (§10.5.13), construction through a bound wrapper
  (§10.4.1.2) and every species construction have to agree with `new`.

  These tests keep the agreement assertions they were written with — a fix that
  reaches Reflect.construct but leaves species construction behind still fails
  here — and now pin the answer they have to agree on: initialized, the same
  value `new` produces. Probed against Node v24.0.1.
*/

const routeResults = (makeSubclass, read) => {
  const Direct = makeSubclass();
  const Proxied = makeSubclass();
  const Bound = makeSubclass();
  return {
    reflect: read(Reflect.construct(Direct, [])),
    proxy: read(Reflect.construct(new Proxy(Proxied, {}), [])),
    bound: read(Reflect.construct(Bound.bind(null), [])),
  };
};

const allAgree = (values) => values.every((v) => v === values[0]);

describe("Array subclass instance elements across Construct routes", () => {
  const makeArraySubclass = () =>
    class Tagged extends Array {
      tag = "t";
    };

  test("`new` initializes the field in both modes", () => {
    const Tagged = makeArraySubclass();
    expect(new Tagged().tag).toBe("t");
    expect(Array.isArray(new Tagged())).toBe(true);
  });

  test("Reflect.construct, proxy and bound routes agree with each other", () => {
    const results = routeResults(makeArraySubclass, (o) => o.tag);
    const values = [results.reflect, results.proxy, results.bound];
    expect(allAgree(values)).toBe(true);
    expect(values[0]).toBe("t");
  });

  test("species construction agrees with the other Construct routes", () => {
    const Tagged = makeArraySubclass();
    const fromTag = Tagged.from([1, 2]).tag;
    const ofTag = Tagged.of(1, 2).tag;
    const mapTag = Tagged.from([1, 2]).map((x) => x).tag;
    const filterTag = Tagged.from([1, 2]).filter(() => true).tag;
    const sliceTag = Tagged.from([1, 2]).slice(0, 1).tag;
    const reflectTag = Reflect.construct(Tagged, []).tag;

    expect(
      allAgree([fromTag, ofTag, mapTag, filterTag, sliceTag, reflectTag]),
    ).toBe(true);
    expect(reflectTag).toBe("t");
  });

  test("the built-in half of the instance is correct on every route", () => {
    const Tagged = makeArraySubclass();
    expect(Array.isArray(Reflect.construct(Tagged, []))).toBe(true);
    expect(Tagged.from([1, 2]).length).toBe(2);
    expect(Tagged.of(1, 2)[1]).toBe(2);
    expect(Tagged.from([1, 2]).map((x) => x * 2)[0]).toBe(2);
  });

  test("a species-constructed instance keeps the subclass prototype", () => {
    const Tagged = makeArraySubclass();
    expect(Tagged.from([1, 2]) instanceof Tagged).toBe(true);
    expect(Tagged.from([1, 2]).map((x) => x) instanceof Tagged).toBe(true);
  });
});

describe("Promise subclass instance elements across Construct routes", () => {
  const makePromiseSubclass = () =>
    class Tagged extends Promise {
      tag = "t";
    };

  test("`new` initializes the field and runs the executor once", () => {
    const Tagged = makePromiseSubclass();
    let runs = 0;
    const instance = new Tagged((resolve) => {
      runs += 1;
      resolve(1);
    });
    expect(instance.tag).toBe("t");
    expect(runs).toBe(1);
  });

  test("an explicit constructor's super() runs the executor once too", () => {
    // The receiver a native super constructor returns replaces the one
    // construction started with, so pre-building it and then calling super()
    // ran the executor twice.
    class Tagged extends Promise {
      tag = "t";

      constructor(executor) {
        super(executor);
      }
    }

    let runs = 0;
    const instance = new Tagged((resolve) => {
      runs += 1;
      resolve(1);
    });
    expect(instance.tag).toBe("t");
    expect(runs).toBe(1);

    let reflectRuns = 0;
    const constructed = Reflect.construct(Tagged, [
      (resolve) => {
        reflectRuns += 1;
        resolve(1);
      },
    ]);
    expect(constructed.tag).toBe("t");
    expect(reflectRuns).toBe(1);
  });

  test("species construction agrees with Reflect.construct", () => {
    const Tagged = makePromiseSubclass();
    const resolveTag = Tagged.resolve(1).tag;
    const thenTag = Tagged.resolve(1).then((v) => v).tag;
    const catchTag = Tagged.resolve(1).catch(() => 1).tag;
    const reflectTag = Reflect.construct(Tagged, [(resolve) => resolve(1)]).tag;

    expect(allAgree([resolveTag, thenTag, catchTag, reflectTag])).toBe(true);
    expect(reflectTag).toBe("t");
  });

  test("the promise half of the instance still settles on every route", () => {
    const Tagged = makePromiseSubclass();
    return Promise.all([
      Tagged.resolve(7),
      Tagged.resolve(7).then((v) => v + 1),
      Reflect.construct(Tagged, [(resolve) => resolve(9)]),
    ]).then((values) => {
      expect(values).toEqual([7, 8, 9]);
    });
  });
});

describe("other built-in chains across Construct routes", () => {
  test("a Map subclass keeps both halves", () => {
    class Tagged extends Map {
      tag = "t";
    }

    const constructed = Reflect.construct(Tagged, [[[1, 2]]]);
    expect(constructed.tag).toBe("t");
    expect(constructed.get(1)).toBe(2);
    expect(constructed instanceof Map).toBe(true);
  });

  test("an Error subclass keeps both halves", () => {
    class Tagged extends Error {
      tag = "t";
    }

    const constructed = Reflect.construct(Tagged, ["boom"]);
    expect(constructed.tag).toBe("t");
    expect(constructed.message).toBe("boom");
    expect(constructed instanceof Error).toBe(true);
  });

  test("a Set subclass keeps both halves", () => {
    class Tagged extends Set {
      tag = "t";
    }

    const constructed = Reflect.construct(Tagged, [[1, 2]]);
    expect(constructed.tag).toBe("t");
    expect(constructed.has(2)).toBe(true);
  });
});

describe("private instance elements on a native chain", () => {
  test("Reflect.construct stamps the brand and runs the initializer", () => {
    class Tagged extends Array {
      #secret = "s";

      read() {
        return this.#secret;
      }
    }

    expect(new Tagged().read()).toBe("s");
    expect(Reflect.construct(Tagged, []).read()).toBe("s");
    expect(Tagged.from([1]).read()).toBe("s");
  });
});

describe("a plain class is unaffected by the native-chain gap", () => {
  test("every Construct route initializes instance elements", () => {
    const makePlain = () =>
      class Plain {
        tag = "t";
      };
    const results = routeResults(makePlain, (o) => o.tag);
    expect(results.reflect).toBe("t");
    expect(results.proxy).toBe("t");
    expect(results.bound).toBe("t");
  });
});
