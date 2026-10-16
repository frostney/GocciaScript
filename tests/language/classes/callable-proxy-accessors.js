/*---
description: An accessor whose get or set is a callable Proxy is called on class instances, arrays and classes
features: [Proxy, class]
---*/

// ES2026 §10.1.8.1 OrdinaryGet step 7 and §10.1.9.2 OrdinarySetWithOwnDescriptor
// step 6 call the accessor's getter or setter, which ToPropertyDescriptor
// requires only to be callable; a Proxy whose target is a function is.
describe("accessors that are callable Proxies", () => {
  test("a getter on an array", () => {
    const array = [1];
    Object.defineProperty(array, "x", { get: new Proxy(() => 12, {}) });
    expect(array.x).toBe(12);
  });

  test("a setter on a class prototype, assigned through an instance", () => {
    const values = [];
    class C {}
    Object.defineProperty(C.prototype, "y", { set: new Proxy((v) => { values.push(v); }, {}) });
    new C().y = 1;
    expect(values).toEqual([1]);
  });

  test("a getter on a class prototype receives the instance", () => {
    class C { constructor() { this.n = 7; } }
    Object.defineProperty(C.prototype, "x", { get: new Proxy(({ m() { return this.n; } }).m, {}) });
    expect(new C().x).toBe(7);
  });

  test("a static getter and setter on a class", () => {
    const values = [];
    class C {}
    Object.defineProperty(C, "x", {
      get: new Proxy(() => 6, {}),
      set: new Proxy((v) => { values.push(v); }, {}),
    });
    expect(C.x).toBe(6);
    C.x = 3;
    expect(values).toEqual([3]);
  });

  test("an inherited getter-only accessor still refuses an assignment", () => {
    class C {}
    Object.defineProperty(C.prototype, "z", { get: new Proxy(() => 1, {}) });
    expect(() => { new C().z = 2; }).toThrow(TypeError);
  });
});
