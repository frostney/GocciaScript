/*---
description: URLSearchParams.prototype.entries
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.entries", () => {
  test("returns an iterator of [name, value] pairs", () => {
    const params = new URLSearchParams("a=1&b=2");
    const entries = [];
    for (const entry of params.entries()) {
      entries.push(entry);
    }
    expect(entries).toEqual([["a", "1"], ["b", "2"]]);
  });

  test("duplicate keys produce separate entries", () => {
    const params = new URLSearchParams("a=1&a=2");
    const entries = [];
    for (const entry of params.entries()) {
      entries.push(entry);
    }
    expect(entries).toEqual([["a", "1"], ["a", "2"]]);
  });

  test("returns empty iterator for empty params", () => {
    const params = new URLSearchParams();
    const entries = [];
    for (const entry of params.entries()) {
      entries.push(entry);
    }
    expect(entries).toEqual([]);
  });

  test("each entry is a two-element array", () => {
    const params = new URLSearchParams("key=val");
    for (const entry of params.entries()) {
      expect(entry.length).toBe(2);
      expect(entry[0]).toBe("key");
      expect(entry[1]).toBe("val");
    }
  });

  test("entries can be spread into an array", () => {
    const params = new URLSearchParams("x=10&y=20");
    const entries = [...params.entries()];
    expect(entries).toEqual([["x", "10"], ["y", "20"]]);
  });

  test("entries values are decoded", () => {
    const params = new URLSearchParams("a=hello+world");
    const entries = [...params.entries()];
    expect(entries[0]).toEqual(["a", "hello world"]);
  });

  test("URLSearchParams is itself iterable and yields entries", () => {
    const params = new URLSearchParams("a=1&b=2");
    const entries = [];
    for (const entry of params) {
      entries.push(entry);
    }
    expect(entries).toEqual([["a", "1"], ["b", "2"]]);
  });
});
