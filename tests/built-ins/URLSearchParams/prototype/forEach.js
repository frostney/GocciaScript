/*---
description: URLSearchParams.prototype.forEach
features: [URLSearchParams]
---*/

describe("URLSearchParams.prototype.forEach", () => {
  test("iterates over all key-value entries", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    const collected = [];
    params.forEach((value, name) => {
      collected.push([name, value]);
    });
    expect(collected).toEqual([["a", "1"], ["b", "2"], ["c", "3"]]);
  });

  test("callback receives value as first argument, name as second", () => {
    const params = new URLSearchParams("key=val");
    params.forEach((value, name) => {
      expect(name).toBe("key");
      expect(value).toBe("val");
    });
  });

  test("callback receives URLSearchParams as third argument", () => {
    const params = new URLSearchParams("a=1");
    params.forEach((value, name, searchParams) => {
      expect(searchParams).toBe(params);
    });
  });

  test("iterates duplicate keys in order", () => {
    const params = new URLSearchParams("a=1&a=2&b=3");
    const names = [];
    const values = [];
    params.forEach((value, name) => {
      names.push(name);
      values.push(value);
    });
    expect(names).toEqual(["a", "a", "b"]);
    expect(values).toEqual(["1", "2", "3"]);
  });

  test("callback not called for empty params", () => {
    const params = new URLSearchParams();
    let called = false;
    params.forEach(() => {
      called = true;
    });
    expect(called).toBe(false);
  });

  test("call count matches number of entries", () => {
    const params = new URLSearchParams("a=1&b=2&c=3");
    let count = 0;
    params.forEach(() => {
      count++;
    });
    expect(count).toBe(3);
  });

  test("throws TypeError when callback is not callable", () => {
    const params = new URLSearchParams("a=1");
    expect(() => {
      params.forEach(null);
    }).toThrow(TypeError);
    expect(() => {
      params.forEach(42);
    }).toThrow(TypeError);
  });

  test("throws TypeError when called with no arguments", () => {
    const params = new URLSearchParams("a=1");
    expect(() => {
      params.forEach();
    }).toThrow(TypeError);
  });
});
