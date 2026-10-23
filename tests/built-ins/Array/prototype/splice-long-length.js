/*---
description: Array.prototype.splice past index 2 ** 31 - 1
features: [Array.prototype.splice]
---*/

// The long-length paths collect sparse indices along the prototype chain,
// which enumerates Array.prototype. The heap-snapshot round-trip mode
// currently fails every object a file passes to expect() once that happens
// (#1901), so these tests assert only on primitives.

// ES2026 §23.1.3.31 Array.prototype.splice: inserting more items than it
// deletes moves each element from k + deleteCount - 1 to k + itemCount - 1,
// for k from len - deleteCount down to start + 1, and ends with
// Set(O, "length", ...), ArraySetLength (§10.4.2.4) on an Array.
describe("splice past index 2 ** 31 - 1", () => {
  test("on an array, inserting moves the later elements up and sets the length", () => {
    const arr = [];
    arr.length = 2 ** 31 + 2;
    arr[2 ** 31 + 1] = "z";
    const removed = arr.splice(2 ** 31, 1, "p", "q");
    expect(removed.length).toBe(1);
    expect(arr.length).toBe(2 ** 31 + 3);
    expect(arr[2 ** 31]).toBe("p");
    expect(arr[2 ** 31 + 1]).toBe("q");
    expect(arr[2 ** 31 + 2]).toBe("z");
  });

  test("on an array-like, inserting moves the elements after the deleted ones", () => {
    const obj = { length: 2 ** 31 + 2, [2 ** 31]: "m", [2 ** 31 + 1]: "z" };
    const removed = Array.prototype.splice.call(obj, 2 ** 31, 1, "p", "q", "s");
    expect(removed.length).toBe(1);
    expect(removed[0]).toBe("m");
    expect(obj.length).toBe(2 ** 31 + 4);
    expect([obj[2 ** 31], obj[2 ** 31 + 1], obj[2 ** 31 + 2], obj[2 ** 31 + 3]].join())
      .toBe("p,q,s,z");
  });

  test("a hole after the deleted elements stays a hole when it moves", () => {
    const arr = [];
    arr.length = 2 ** 31 + 4;
    arr[2 ** 31 + 1] = "h1";
    arr[2 ** 31 + 3] = "h3";
    arr.splice(2 ** 31 + 1, 1, "x", "y");
    expect(arr.length).toBe(2 ** 31 + 5);
    expect(Object.keys(arr).join()).toBe([2 ** 31 + 1, 2 ** 31 + 2, 2 ** 31 + 4].join());
    expect(arr[2 ** 31 + 4]).toBe("h3");
  });
});
