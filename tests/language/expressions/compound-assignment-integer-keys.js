/*---
description: Compound assignment and update on an integer element key use that key's string form
features: [compound-assignment-operators, Proxy, TypedArray, accessor-properties]
---*/

// ES2026 §13.15.2 and §13.4.2.1 evaluate `a[k] op= v` and `a[k]++` through a
// property reference whose key is ToPropertyKey(k), taken once. For an integer
// k that key is ToString(k), so every receiver sees "1", never the number 1.

describe("compound assignment with an integer element key", () => {
  test("a Proxy's get and set traps both receive the string key", () => {
    const seen = [];
    const target = [10, 20, 30];
    const proxy = new Proxy(target, {
      get(t, key, receiver) {
        seen.push(`get ${typeof key} ${key}`);
        return Reflect.get(t, key, receiver);
      },
      set(t, key, value, receiver) {
        seen.push(`set ${typeof key} ${key}`);
        return Reflect.set(t, key, value, receiver);
      },
    });

    proxy[1] += 5;
    proxy[2]++;
    proxy[0] ??= 1;

    expect(seen).toEqual([
      "get string 1",
      "set string 1",
      "get string 2",
      "set string 2",
      "get string 0",
    ]);
    expect(target).toEqual([10, 25, 31]);
  });

  test("an accessor named by the integer's string form runs for both halves", () => {
    const stored = [];
    const holder = {
      get 1() {
        return 4;
      },
      set 1(value) {
        stored.push(value);
      },
    };

    holder[1] *= 2;
    holder[1]--;

    expect(stored).toEqual([8, 3]);
  });

  test("arrays, holes, typed arrays and negative keys update in place", () => {
    const values = [1, , 3];
    values[1] += 1;
    values[2] -= 1;
    expect(Number.isNaN(values[1])).toBe(true);
    expect(values[2]).toBe(2);

    const bytes = new Int8Array(2);
    bytes[0] += 300;
    bytes[1]--;
    bytes[5] += 1;
    expect([bytes[0], bytes[1], bytes[5]]).toEqual([44, -1, undefined]);

    const negative = { "-1": 1 };
    negative[-1] += 1;
    expect(negative["-1"]).toBe(2);
  });

  test("a key object is converted once, before the read", () => {
    let conversions = 0;
    const key = {
      toString() {
        conversions++;
        return "0";
      },
    };
    const values = [5];

    values[key] += 1;
    values[key]++;

    expect(values[0]).toBe(7);
    expect(conversions).toBe(2);
  });
});
