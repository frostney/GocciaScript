/*---
description: Every route through the abstract Construct operation treats a class whose chain reaches a built-in identically (known gap, see below)
features: [Reflect]
---*/

/*
  KNOWN GAP, pinned deliberately.

  A class whose superclass chain reaches a built-in constructor is declined by
  RedirectEvaluatorClassConstruct (Goccia.Evaluator.pas) and built by
  TGocciaClassValue.Instantiate instead, which runs no instance elements. In
  interpreted mode that means every route through the abstract Construct
  operation — Reflect.construct, a proxy without a construct trap, a bound
  class, and every species construction (Array.from, Array.prototype.map,
  Promise.prototype.then, ...) — hands back an instance whose declared fields
  are undefined. `new` is unaffected: it calls InstantiateClass directly.
  Bytecode mode is correct throughout, so this is also a mode divergence.

  The guard cannot simply be widened: InstantiateClass resolves
  newTarget.prototype before ArrayBuffer/SharedArrayBuffer/DataView validate
  their arguments, and it builds the native receiver twice for a derived class
  whose constructor calls super() explicitly (a Promise executor runs twice).
  Both have to be fixed there first.

  So these tests do not assert a value that differs per mode. They assert the
  property that must survive any fix, partial or complete: all the Construct
  routes agree with each other, and the answer is either "the field is
  initialized" (fixed, and what `new` already does) or "no route initializes
  it" (today's gap). A fix that reaches Reflect.construct but leaves species
  construction behind — or the reverse — fails here.
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
    // Either all initialized (fixed) or none (the pinned gap).
    expect(values[0] === "t" || values[0] === undefined).toBe(true);
  });

  test("species construction agrees with the other Construct routes", () => {
    const Tagged = makeArraySubclass();
    const fromTag = Tagged.from([1, 2]).tag;
    const ofTag = Tagged.of(1, 2).tag;
    const mapTag = Tagged.from([1, 2]).map((x) => x).tag;
    const reflectTag = Reflect.construct(Tagged, []).tag;

    expect(allAgree([fromTag, ofTag, mapTag, reflectTag])).toBe(true);
    expect(reflectTag === "t" || reflectTag === undefined).toBe(true);
  });

  test("the built-in half of the instance is correct on every route", () => {
    const Tagged = makeArraySubclass();
    expect(Array.isArray(Reflect.construct(Tagged, []))).toBe(true);
    expect(Tagged.from([1, 2]).length).toBe(2);
    expect(Tagged.of(1, 2)[1]).toBe(2);
    expect(Tagged.from([1, 2]).map((x) => x * 2)[0]).toBe(2);
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

  test("species construction agrees with Reflect.construct", () => {
    const Tagged = makePromiseSubclass();
    const resolveTag = Tagged.resolve(1).tag;
    const thenTag = Tagged.resolve(1).then((v) => v).tag;
    const reflectTag = Reflect.construct(Tagged, [(resolve) => resolve(1)]).tag;

    expect(allAgree([resolveTag, thenTag, reflectTag])).toBe(true);
    expect(reflectTag === "t" || reflectTag === undefined).toBe(true);
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
