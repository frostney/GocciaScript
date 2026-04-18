/*---
description: Headers.prototype.entries / keys / values / Symbol.iterator
features: [fetch]
---*/

describe("Headers.prototype.entries", () => {
  test("returns an iterator of [name, value] pairs", () => {
    const h = new Headers({ "X-A": "1", "X-B": "2" });
    const entries = [...h.entries()];
    expect(entries).toEqual([["x-a", "1"], ["x-b", "2"]]);
  });
});

describe("Headers.prototype.keys", () => {
  test("returns an iterator of header names", () => {
    const h = new Headers({ "X-A": "1", "X-B": "2" });
    const keys = [...h.keys()];
    expect(keys).toEqual(["x-a", "x-b"]);
  });
});

describe("Headers.prototype.values", () => {
  test("returns an iterator of header values", () => {
    const h = new Headers({ "X-A": "1", "X-B": "2" });
    const values = [...h.values()];
    expect(values).toEqual(["1", "2"]);
  });
});

describe("Headers[Symbol.iterator]", () => {
  test("is the same as entries", () => {
    const h = new Headers({ "X-A": "1" });
    const fromIter = [...h];
    const fromEntries = [...h.entries()];
    expect(fromIter).toEqual(fromEntries);
  });

  test("works in for...of", () => {
    const h = new Headers({ "X-A": "1", "X-B": "2" });
    const collected = [];
    for (const [name, value] of h) {
      collected.push(name + "=" + value);
    }
    expect(collected).toEqual(["x-a=1", "x-b=2"]);
  });
});
