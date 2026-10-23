/*---
description: >
  Reading Array.prototype.toLocaleString off the prototype, and nothing
  else of its shim, leaves the objects a file observes round-tripping in
  the test runner's heap-snapshot mode (#1901).
features: [Array.prototype.toLocaleString]
---*/

// Array.prototype.toLocaleString and Number.prototype.toLocaleString share
// one lazily loaded shim. This file builds only the Array member: neither
// Number.prototype.toLocaleString nor a call that reaches it runs here.
describe("Array.prototype.toLocaleString read off the prototype", () => {
  test("observed objects", () => {
    expect([1, 2]).toEqual([1, 2]);
    expect({ a: 1 }).toEqual({ a: 1 });
  });

  test("reads the method", () => {
    expect(typeof Array.prototype.toLocaleString).toBe("function");
    expect(Object.hasOwn(Array.prototype, "toLocaleString")).toBe(true);
  });
});
