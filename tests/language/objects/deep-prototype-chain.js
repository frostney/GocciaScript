/*---
description: Reads, in checks and assignments walk a prototype chain of any length
features: [prototype-chain, Object.create]
---*/

// The specification puts no limit on the length of a prototype chain.
const chainOf = (length, base = {}) => {
  let object = base;
  for (const i of Array.from({ length })) {
    object = Object.create(object);
  }
  return object;
};

describe("a deep prototype chain", () => {
  test("a missing property reads as undefined through 20,000 links", () => {
    expect(chainOf(20000).missing).toBeUndefined();
  });

  test("a property at the far end is found through 20,000 links", () => {
    const deep = chainOf(20000, { far: "end" });
    expect(deep.far).toBe("end");
    expect("far" in deep).toBe(true);
    expect("missing" in deep).toBe(false);
  });

  test("a symbol-keyed property at the far end is found through 20,000 links", () => {
    const key = Symbol("key");
    expect(chainOf(20000, { [key]: "end" })[key]).toBe("end");
  });

  test("an object with a 20,000-link chain converts to a string", () => {
    expect(String(chainOf(20000))).toBe("[object Object]");
  });

  test("an assignment through 300 links creates an own property", () => {
    const deep = chainOf(300);
    deep.x = 1;
    expect(deep.x).toBe(1);
    expect(Object.hasOwn(deep, "x")).toBe(true);
  });

  test("an assignment through 20,000 links creates an own property", () => {
    const deep = chainOf(20000);
    deep.x = 1;
    expect(Object.hasOwn(deep, "x")).toBe(true);
    const key = Symbol("key");
    deep[key] = 2;
    expect(deep[key]).toBe(2);
    expect(Reflect.set(deep, "y", 3)).toBe(true);
    expect(deep.y).toBe(3);
  });

  test("an assignment through 300 links calls an inherited setter at the far end", () => {
    let seen;
    const deep = chainOf(300, { set x(value) { seen = value; } });
    deep.x = 5;
    expect(seen).toBe(5);
    expect(Object.hasOwn(deep, "x")).toBe(false);
  });

  test("an assignment through 300 links to an inherited read-only property throws", () => {
    const deep = chainOf(300, Object.defineProperty({}, "x", { value: 1, writable: false }));
    expect(() => { deep.x = 2; }).toThrow(TypeError);
    expect(Object.hasOwn(deep, "x")).toBe(false);
  });
});

describe("a deep chain of objects whose class answers [[Get]] itself", () => {
  test("a static method is found through 1,500 classes", () => {
    class Base { static found() { return "base"; } }
    let Derived = Base;
    for (const i of Array.from({ length: 1500 })) {
      Derived = class extends Derived {};
    }
    expect(Derived.found()).toBe("base");
  });

  test("an array method is found through 1,500 arrays", () => {
    let array = [];
    for (const i of Array.from({ length: 1500 })) {
      array = Object.setPrototypeOf([], array);
    }
    expect(typeof array.push).toBe("function");
    expect(array.missing).toBeUndefined();
  });
});
