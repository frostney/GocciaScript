/*---
description: Headers.prototype.forEach
features: [fetch]
---*/

describe("Headers.prototype.forEach", () => {
  test("iterates over all entries", () => {
    const h = new Headers({ "X-A": "1", "X-B": "2" });
    const collected = [];
    h.forEach((value, name) => {
      collected.push([name, value]);
    });
    expect(collected).toEqual([["x-a", "1"], ["x-b", "2"]]);
  });

  test("callback receives value as first argument, name as second", () => {
    const h = new Headers({ Key: "val" });
    h.forEach((value, name) => {
      expect(name).toBe("key");
      expect(value).toBe("val");
    });
  });

  test("callback receives Headers as third argument", () => {
    const h = new Headers({ A: "1" });
    h.forEach((value, name, headers) => {
      expect(headers).toBe(h);
    });
  });

  test("callback not called for empty headers", () => {
    const fn = mock();
    const h = new Headers();
    h.forEach(fn);
    expect(fn).toHaveBeenCalledTimes(0);
  });

  test("throws TypeError when callback is not callable", () => {
    const h = new Headers({ A: "1" });
    expect(() => h.forEach(null)).toThrow(TypeError);
    expect(() => h.forEach(42)).toThrow(TypeError);
  });
});
