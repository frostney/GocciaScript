/*---
description: A built-in reached through a subclass falls back to its own intrinsic prototype when new.target.prototype is not an object
features: [class, Reflect, TypedArray, ArrayBuffer, SharedArrayBuffer, DataView, Map, Set, WeakMap, WeakSet, WeakRef, FinalizationRegistry]
---*/

// ES2026 §13.3.7.1 SuperCall calls Construct(func, argList, newTarget), and the
// built-in allocates through §10.1.14 GetPrototypeFromConstructor: when
// newTarget.prototype is not an object, step 3 uses the built-in's own
// intrinsic (%Map.prototype%, %Uint8Array.prototype%, ...) from newTarget's
// realm, never %Object.prototype%. A bound function has no `prototype`
// property, which makes it the simplest such newTarget. Expected values from
// Node.js v24.

const boundNewTarget = (class {}).bind(null);

const cases = [
  ["Array", Array, [2], (o) => o.length, 2],
  ["Uint8Array", Uint8Array, [2], (o) => o.length, 2],
  ["Float64Array", Float64Array, [3], (o) => o.length, 3],
  ["ArrayBuffer", ArrayBuffer, [8], (o) => o.byteLength, 8],
  ["SharedArrayBuffer", SharedArrayBuffer, [8], (o) => o.byteLength, 8],
  ["DataView", DataView, [new ArrayBuffer(4)], (o) => o.byteLength, 4],
  ["Map", Map, [[[1, 2]]], (o) => o.size, 1],
  ["Set", Set, [[1, 2]], (o) => o.size, 2],
  ["WeakMap", WeakMap, [[[{}, 1]]], (o) => typeof o.set, "function"],
  ["WeakSet", WeakSet, [[{}]], (o) => typeof o.add, "function"],
  ["WeakRef", WeakRef, [{}], (o) => typeof o.deref, "function"],
  ["FinalizationRegistry", FinalizationRegistry, [() => {}], (o) => typeof o.register, "function"],
  ["String", String, ["xy"], (o) => o.length, 2],
  ["Number", Number, [3], (o) => o.valueOf(), 3],
  ["Boolean", Boolean, [true], (o) => o.valueOf(), true],
];

describe("a subclass with its own constructor", () => {
  cases.forEach(([name, Base, args, read, expected]) => {
    test(`${name}: super() gives the instance ${name}.prototype`, () => {
      class Sub extends Base {
        constructor(...a) {
          super(...a);
        }
      }
      const instance = Reflect.construct(Sub, args, boundNewTarget);
      expect(Object.getPrototypeOf(instance)).toBe(Base.prototype);
      expect(read(instance)).toBe(expected);
    });
  });
});

describe("a subclass with an implicit constructor", () => {
  cases.forEach(([name, Base, args, read, expected]) => {
    test(`${name}: the instance gets ${name}.prototype`, () => {
      class Sub extends Base {}
      const instance = Reflect.construct(Sub, args, boundNewTarget);
      expect(Object.getPrototypeOf(instance)).toBe(Base.prototype);
      expect(read(instance)).toBe(expected);
    });
  });
});

describe("a subclass reaching the built-in through an intermediate class", () => {
  test("Map: the leaf's super() still falls back to Map.prototype", () => {
    class Middle extends Map {}
    class Leaf extends Middle {
      constructor(entries) {
        super(entries);
      }
    }
    const instance = Reflect.construct(Leaf, [[[1, 2]]], boundNewTarget);
    expect(Object.getPrototypeOf(instance)).toBe(Map.prototype);
    expect(instance.size).toBe(1);
  });

  test("Uint8Array: the leaf's super() still falls back to Uint8Array.prototype", () => {
    class Middle extends Uint8Array {}
    class Leaf extends Middle {
      constructor(length) {
        super(length);
      }
    }
    const instance = Reflect.construct(Leaf, [3], boundNewTarget);
    expect(Object.getPrototypeOf(instance)).toBe(Uint8Array.prototype);
    expect(instance.length).toBe(3);
  });

  test("Array: the built-in's elements are added once", () => {
    class Middle extends Array {}
    class Leaf extends Middle {
      constructor(...items) {
        super(...items);
      }
    }
    const instance = new Leaf(1, 2, 3);
    expect(instance.length).toBe(3);
    expect([...instance]).toEqual([1, 2, 3]);
    expect(instance instanceof Leaf).toBe(true);
    expect(Array.isArray(instance)).toBe(true);
  });
});

describe("a new.target whose prototype is an object", () => {
  test("still wins over the built-in's own prototype", () => {
    class Sub extends Map {
      constructor() {
        super();
      }
    }
    class Other {}
    const instance = Reflect.construct(Sub, [], Other);
    expect(Object.getPrototypeOf(instance)).toBe(Other.prototype);
  });
});
