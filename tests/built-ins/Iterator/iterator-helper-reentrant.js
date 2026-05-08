/*---
description: Iterator helpers throw TypeError on re-entrant .next() (ES2026 §27.1.2.1.1)
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
