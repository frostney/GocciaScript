/*---
description: URLSearchParams.prototype.size
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.size", () => {
  test("is 0 for empty params", () => {
    const params = new URLSearchParams();
    expect(params.size).toBe(0);
  });

  test("returns number of entries", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    expect(params.size).toBe(3);
  });

  test("counts duplicate keys as separate entries", () => {
    const params = new URLSearchParams("a=1&a=2&a=3");
    expect(params.size).toBe(3);
  });

  test("increases after append", () => {
    const params = new URLSearchParams("a=1");
    params.append("b", "2");
    expect(params.size).toBe(2);
  });

  test("decreases after delete (removes all with that key)", () => {
    const params = new URLSearchParams("a=1&a=2&b=3");
    params.delete("a");
    expect(params.size).toBe(1);
  });

  test("decreases when set removes duplicate entries", () => {
    const params = new URLSearchParams("a=1&a=2&a=3");
    params.set("a", "x");
    expect(params.size).toBe(1);
  });

  test("is 0 after deleting all entries", () => {
    const params = new URLSearchParams("a=1&b=2");
    params.delete("a");
    params.delete("b");
    expect(params.size).toBe(0);
  });

  test("counts entries from array constructor", () => {
    const params = new URLSearchParams([["a", "1"], ["a", "2"], ["b", "3"]]);
    expect(params.size).toBe(3);
  });
});
