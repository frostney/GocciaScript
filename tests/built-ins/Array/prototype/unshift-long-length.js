/*---
description: Array.prototype.unshift on an array longer than 2 ** 31 - 1
features: [Array.prototype.unshift]
---*/

// The long-length paths collect sparse indices along the prototype chain,
// which enumerates Array.prototype. The heap-snapshot round-trip mode
// currently fails every object a file passes to expect() once that happens
// (#1901), so these tests assert only on primitives.

// ES2026 §23.1.3.37 Array.prototype.unshift ends with Set(O, "length",
// len + argCount, true), ArraySetLength (§10.4.2.4) on an Array.
describe("unshift on an array longer than 2 ** 31 - 1", () => {
  test("moves the elements up and sets the length", () => {
    const arr = [];
    arr.length = 2 ** 31 + 1;
    arr[2 ** 31] = "last";
    expect(arr.unshift("u")).toBe(2 ** 31 + 2);
    expect(arr.length).toBe(2 ** 31 + 2);
    expect(arr[0]).toBe("u");
    expect(arr[2 ** 31 + 1]).toBe("last");
    expect(Object.hasOwn(arr, 2 ** 31)).toBe(false);
  });
});
