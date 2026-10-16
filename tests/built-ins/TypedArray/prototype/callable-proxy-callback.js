/*---
description: TypedArray methods accept a callable Proxy as their callback
features: [TypedArray, Proxy]
---*/

// The callback only has to be callable (for example ES2026 §23.2.3.22
// %TypedArray%.prototype.map step 3), and a Proxy whose target is a function
// is. Expected values match Node.js 24.
const proxied = (fn) => new Proxy(fn, {});

describe("TypedArray callbacks that are callable Proxies", () => {
  test("map and filter", () => {
    expect(Array.from(new Uint8Array([1, 2]).map(proxied((x) => x * 2)))).toEqual([2, 4]);
    expect(Array.from(new Uint8Array([1, 3, 2]).filter(proxied((x) => x > 1)))).toEqual([3, 2]);
  });

  test("forEach passes the element, the index and the array", () => {
    const array = new Uint8Array([5, 6]);
    const seen = [];
    array.forEach(proxied((value, index, self) => seen.push([value, index, self === array])));
    expect(seen).toEqual([[5, 0, true], [6, 1, true]]);
  });

  test("forEach passes thisArg", () => {
    const thisArg = { tag: "t" };
    let received;
    new Uint8Array([1]).forEach(proxied(({ m() { received = this; } }).m), thisArg);
    expect(received).toBe(thisArg);
  });

  test("every, some, find, findIndex, findLast and findLastIndex", () => {
    const array = new Uint8Array([1, 3, 2]);
    expect(array.every(proxied((x) => x > 0))).toBe(true);
    expect(array.some(proxied((x) => x > 2))).toBe(true);
    expect(array.find(proxied((x) => x > 2))).toBe(3);
    expect(array.findIndex(proxied((x) => x > 2))).toBe(1);
    expect(array.findLast(proxied((x) => x > 0))).toBe(2);
    expect(array.findLastIndex(proxied((x) => x > 0))).toBe(2);
  });

  test("reduce, reduceRight, sort, toSorted and from", () => {
    const array = new Uint8Array([1, 3, 2]);
    expect(array.reduce(proxied((a, b) => a + b))).toBe(6);
    expect(array.reduceRight(proxied((a, b) => a + b))).toBe(6);
    expect(Array.from(array.toSorted(proxied((a, b) => b - a)))).toEqual([3, 2, 1]);
    expect(Array.from(new Uint8Array([1, 3, 2]).sort(proxied((a, b) => b - a)))).toEqual([3, 2, 1]);
    expect(Array.from(Uint8Array.from([1, 2], proxied((x) => x + 1)))).toEqual([2, 3]);
  });

  test("the Proxy's apply trap runs for each element", () => {
    let calls = 0;
    const callback = new Proxy((x) => x, { apply: (target, thisArg, args) => { calls += 1; return target(...args) + 10; } });
    expect(Array.from(new Uint8Array([1, 2]).map(callback))).toEqual([11, 12]);
    expect(calls).toBe(2);
  });

  test("an error thrown by the callback propagates", () => {
    expect(() => new Uint8Array([1]).forEach(proxied(() => { throw new SyntaxError("x"); }))).toThrow(SyntaxError);
  });
});
