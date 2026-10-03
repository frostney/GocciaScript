/*---
description: >
  A constructor body's super(...) builds a built-in receiver from its own
  arguments, not from the arguments passed to new
features: [class, class-fields-public, class-fields-private, class-inheritance, TypedArray, ArrayBuffer, SharedArrayBuffer, resizable-arraybuffer, Reflect, Symbol.iterator]
---*/

// ES2026 §13.3.7.1 SuperCall evaluates its own argument list and calls
// Construct(superConstructor, argList, newTarget); the built-in constructor
// sees only those arguments (§23.2.5.1 TypedArray, §25.1.4.1 ArrayBuffer,
// §25.2.3.1 SharedArrayBuffer).

describe("typed array subclasses", () => {
  test("the receiver is built from the super() arguments", () => {
    class Two extends Float64Array { constructor() { super(2); } }
    class Five extends Uint8Array { constructor(n) { super(5); } }
    class Second extends Uint8Array { constructor(a, b) { super(b); } }
    class FromList extends Uint8Array { constructor() { super([1, 2, 3]); } }
    class View extends Uint8Array { constructor() { super(new ArrayBuffer(8), 2, 3); } }

    expect(new Two().length).toBe(2);
    expect(new Five(2).length).toBe(5);
    expect(new Second(2, 7).length).toBe(7);
    expect(Array.from(new FromList())).toEqual([1, 2, 3]);
    const view = new View();
    expect([view.length, view.byteOffset, view.buffer.byteLength]).toEqual([3, 2, 8]);
  });

  test("the new arguments are not validated or iterated", () => {
    let iterated = 0;
    const iterable = { *[Symbol.iterator]() { iterated++; yield 1; } };
    class Fixed extends Uint8Array { constructor() { super(4); } }

    expect(new Fixed(-1).length).toBe(4);
    expect(new Fixed(iterable).length).toBe(4);
    expect(iterated).toBe(0);
  });

  test("the receiver is an instance of the subclass and fields see the built-in", () => {
    class Tagged extends Uint16Array {
      size = this.length;
      #secret = "s";
      constructor() { super(3); }
      secret() { return this.#secret; }
    }
    const tagged = new Tagged(9);

    expect(tagged instanceof Tagged).toBe(true);
    expect(tagged instanceof Uint16Array).toBe(true);
    expect(tagged.size).toBe(3);
    expect(tagged.secret()).toBe("s");
  });

  test("super() from an arrow function rebinds the constructor's this", () => {
    class Arrow extends Int8Array {
      constructor() {
        const callSuper = () => super(6);
        callSuper();
        this.after = this.length;
      }
    }
    const arrow = new Arrow(1);

    expect(arrow.length).toBe(6);
    expect(arrow.after).toBe(6);
  });

  test("an intermediate constructor and an implicit one pass their own arguments on", () => {
    class Base extends Uint8Array { constructor() { super(2); } }
    class Implicit extends Base {}
    class Explicit extends Base { constructor() { super(100); } }

    expect(new Implicit(9).length).toBe(2);
    expect(new Implicit(-1).length).toBe(2);
    expect(new Explicit(9).length).toBe(2);
  });

  test("the prototype comes from new.target", () => {
    class Two extends Uint8Array { constructor() { super(2); } }
    class Other {}
    const made = Reflect.construct(Two, [9], Other);

    expect(Object.getPrototypeOf(made)).toBe(Other.prototype);
    expect(Uint8Array.prototype.slice.call(made).length).toBe(2);
  });

  test("Reflect.construct and a bound constructor pass their arguments to the constructor body only", () => {
    let iterated = 0;
    const iterable = { *[Symbol.iterator]() { iterated++; yield 1; } };
    class Fixed extends Uint8Array { constructor() { super(4); } }
    class Inherited extends Fixed {}
    const Bound = Fixed.bind(null);

    expect(Reflect.construct(Fixed, [-1]).length).toBe(4);
    expect(Reflect.construct(Fixed, [iterable]).length).toBe(4);
    expect(Reflect.construct(Inherited, [-1]).length).toBe(4);
    expect(new Bound(-1).length).toBe(4);
    expect(iterated).toBe(0);
  });

  test("a second super() throws and keeps the first receiver", () => {
    class Twice extends Uint8Array {
      constructor() {
        super(1);
        try {
          super(2);
        } catch (error) {
          this.error = error.constructor;
        }
      }
    }
    const twice = new Twice();

    expect(twice.length).toBe(1);
    expect(twice.error).toBe(ReferenceError);
  });
});

describe("buffer subclasses", () => {
  test("an ArrayBuffer subclass is built from the super() arguments", () => {
    class Eight extends ArrayBuffer { constructor() { super(8); } }
    class Resizable extends ArrayBuffer { constructor() { super(4, { maxByteLength: 16 }); } }
    const resizable = new Resizable(99);

    expect(new Eight().byteLength).toBe(8);
    expect([resizable.byteLength, resizable.maxByteLength, resizable.resizable]).toEqual([4, 16, true]);
    expect(new Eight(-1).byteLength).toBe(8);
    expect(Reflect.construct(Eight, [-1]).byteLength).toBe(8);
  });

  test("a SharedArrayBuffer subclass is built from the super() arguments", () => {
    class Eight extends SharedArrayBuffer { constructor() { super(8); } }
    class Growable extends SharedArrayBuffer { constructor() { super(2, { maxByteLength: 6 }); } }

    expect(new Eight(1).byteLength).toBe(8);
    expect(new Growable().maxByteLength).toBe(6);
  });
});

describe("other built-ins built at super()", () => {
  test("Map, Set, Array and DataView subclasses keep the super() arguments", () => {
    class PairMap extends Map { constructor() { super([[1, 2]]); } }
    class PairSet extends Set { constructor() { super([1, 2]); } }
    class Triple extends Array { constructor() { super(1, 2, 3); } }
    class Window extends DataView { constructor() { super(new ArrayBuffer(8), 2); } }

    expect(new PairMap([[3, 4], [5, 6]]).size).toBe(1);
    expect(new PairSet([9]).size).toBe(2);
    expect(Array.from(new Triple(7))).toEqual([1, 2, 3]);
    expect(new Window().byteLength).toBe(6);
  });
});
