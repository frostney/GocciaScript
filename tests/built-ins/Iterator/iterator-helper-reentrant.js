/*---
description: Iterator helpers throw TypeError on re-entrant .next() and chained helpers isolate per-instance guards
features: [Iterator, iterator-helpers, iterator-sequencing]
---*/

const makeReentrantIterable = (getHelper) => ({
  [Symbol.iterator]() {
    return {
      next() {
        getHelper().next();
        return { done: false, value: 1 };
      },
    };
  },
});

describe("Iterator helper re-entrancy guard", () => {
  test.each([
    [
      "Iterator.concat",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h));
        return h;
      },
    ],
    [
      "Iterator.prototype.map",
      () => {
        let h;
        h = [1, 2].values().map(() => { h.next(); return 0; });
        return h;
      },
    ],
    [
      "Iterator.prototype.filter",
      () => {
        let h;
        h = [1, 2].values().filter(() => { h.next(); return true; });
        return h;
      },
    ],
    [
      "Iterator.prototype.flatMap",
      () => {
        let h;
        h = [1, 2].values().flatMap(() => { h.next(); return [0]; });
        return h;
      },
    ],
    [
      "Iterator.prototype.take",
      () => {
        let h;
        const src = makeReentrantIterable(() => h);
        h = Iterator.from(src).take(5);
        return h;
      },
    ],
    [
      "Iterator.prototype.drop",
      () => {
        let h;
        const src = makeReentrantIterable(() => h);
        h = Iterator.from(src).drop(0);
        return h;
      },
    ],
    [
      "Iterator.zip",
      () => {
        let h;
        const src = makeReentrantIterable(() => h);
        h = Iterator.zip([src]);
        return h;
      },
    ],
    [
      "Iterator.zipKeyed",
      () => {
        let h;
        const src = makeReentrantIterable(() => h);
        h = Iterator.zipKeyed({ a: src });
        return h;
      },
    ],
  ])("%s throws TypeError on re-entrant .next()", (_name, factory) => {
    const helper = factory();
    expect(() => helper.next()).toThrow(TypeError);
  });
});

describe("Chained iterator helper re-entrancy", () => {
  test.each([
    [
      "concat → take",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h)).take(5);
        return h;
      },
    ],
    [
      "concat → map",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h)).map(x => x);
        return h;
      },
    ],
    [
      "concat → filter",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h)).filter(() => true);
        return h;
      },
    ],
    [
      "zip → take",
      () => {
        let h;
        h = Iterator.zip([makeReentrantIterable(() => h)]).take(5);
        return h;
      },
    ],
    [
      "zip → map",
      () => {
        let h;
        h = Iterator.zip([makeReentrantIterable(() => h)]).map(x => x);
        return h;
      },
    ],
    [
      "concat → drop",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h)).drop(0);
        return h;
      },
    ],
    [
      "concat → flatMap",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h)).flatMap(x => [x]);
        return h;
      },
    ],
    [
      "concat → filter → take (three-deep)",
      () => {
        let h;
        h = Iterator.concat(makeReentrantIterable(() => h)).filter(() => true).take(5);
        return h;
      },
    ],
  ])("%s throws TypeError when source re-enters outermost helper", (_name, factory) => {
    const helper = factory();
    expect(() => helper.next()).toThrow(TypeError);
  });

  test.each([
    [
      "map → filter (predicate re-enters map)",
      () => {
        const inner = [1, 2, 3].values().map(x => x * 2);
        return inner.filter(() => { inner.next(); return true; });
      },
    ],
    [
      "filter → map (mapper re-enters filter)",
      () => {
        const inner = [1, 2, 3].values().filter(() => true);
        return inner.map(x => { inner.next(); return x; });
      },
    ],
    [
      "take → map (mapper re-enters take)",
      () => {
        const inner = [1, 2, 3].values().take(5);
        return inner.map(x => { inner.next(); return x; });
      },
    ],
    [
      "concat → filter (predicate re-enters concat)",
      () => {
        const inner = Iterator.concat([1, 2, 3]);
        return inner.filter(() => { inner.next(); return true; });
      },
    ],
    [
      "zip → map (mapper re-enters zip)",
      () => {
        const inner = Iterator.zip([[1, 2, 3]]);
        return inner.map(x => { inner.next(); return x; });
      },
    ],
    [
      "drop → flatMap (flatMap callback re-enters drop)",
      () => {
        const inner = [1, 2, 3].values().drop(0);
        return inner.flatMap(x => { inner.next(); return [x]; });
      },
    ],
    [
      "map → filter → take (predicate re-enters map, three-deep)",
      () => {
        const inner = [1, 2, 3, 4, 5, 6].values().map(x => x);
        return inner.filter(() => { inner.next(); return true; }).take(5);
      },
    ],
  ])("%s does not throw (per-instance guard isolation)", (_name, factory) => {
    const chain = factory();
    const { done } = chain.next();
    expect(done).toBe(false);
  });
});
