/*---
description: Assignment to an own accessor property calls its setter, or fails when it has none, on every kind of object
features: [Object.defineProperty, accessor-properties, class, TypedArray, Map, Set, WeakMap, Reflect.set, destructuring]
---*/

// ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor steps 3-7: a property that is
// an own accessor of the receiver is assigned by calling its setter. With no
// setter [[Set]] returns false, which §6.2.5.6 PutValue step 3.e turns into a
// TypeError in strict code. Either way the accessor stays an accessor.

class Plain {}
class Derived extends Plain {}
class WithField {
  kept = 1;
}
class ExtendsObject extends Object {}
class ExtendsArray extends Array {}
class ExtendsUint8Array extends Uint8Array {}
class ExtendsMap extends Map {}
class ExtendsSet extends Set {}
class ExtendsWeakMap extends WeakMap {}
class ExtendsArrayBuffer extends ArrayBuffer {}
class ExtendsDataView extends DataView {}
class ExtendsString extends String {}
class ExtendsNumber extends Number {}
class ExtendsBoolean extends Boolean {}
class ExtendsDate extends Date {}
class ExtendsRegExp extends RegExp {}
class ExtendsError extends Error {}
class ExtendsPromise extends Promise {}

const receivers = [
  ["a plain object", () => ({})],
  ["a class instance", () => new Plain()],
  ["an instance of a derived class", () => new Derived()],
  ["an instance with a field", () => new WithField()],
  ["an instance of a class extending Object", () => new ExtendsObject()],
  ["an array", () => [1, 2, 3]],
  ["an instance of a class extending Array", () => new ExtendsArray()],
  ["a Uint8Array", () => new Uint8Array(2)],
  ["a Float64Array", () => new Float64Array(2)],
  ["a BigInt64Array", () => new BigInt64Array(2)],
  ["an instance of a class extending Uint8Array", () => new ExtendsUint8Array(2)],
  ["a Map", () => new Map()],
  ["an instance of a class extending Map", () => new ExtendsMap()],
  ["a Set", () => new Set()],
  ["an instance of a class extending Set", () => new ExtendsSet()],
  ["a WeakMap", () => new WeakMap()],
  ["an instance of a class extending WeakMap", () => new ExtendsWeakMap()],
  ["a WeakSet", () => new WeakSet()],
  ["a WeakRef", () => new WeakRef({})],
  ["a FinalizationRegistry", () => new FinalizationRegistry(() => {})],
  ["an ArrayBuffer", () => new ArrayBuffer(4)],
  ["an instance of a class extending ArrayBuffer", () => new ExtendsArrayBuffer(4)],
  ["a SharedArrayBuffer", () => new SharedArrayBuffer(4)],
  ["a DataView", () => new DataView(new ArrayBuffer(4))],
  ["an instance of a class extending DataView", () => new ExtendsDataView(new ArrayBuffer(4))],
  ["a String object", () => new String("ab")],
  ["an instance of a class extending String", () => new ExtendsString("ab")],
  ["a Number object", () => new Number(1)],
  ["an instance of a class extending Number", () => new ExtendsNumber(1)],
  ["a Boolean object", () => new Boolean(true)],
  ["an instance of a class extending Boolean", () => new ExtendsBoolean(true)],
  ["a BigInt object", () => Object(1n)],
  ["a Date", () => new Date(0)],
  ["an instance of a class extending Date", () => new ExtendsDate(0)],
  ["a RegExp", () => /a/],
  ["an instance of a class extending RegExp", () => new ExtendsRegExp("a")],
  ["an Error", () => new Error("e")],
  ["an instance of a class extending Error", () => new ExtendsError("e")],
  ["a Promise", () => Promise.resolve(1)],
  ["an instance of a class extending Promise", () => ExtendsPromise.resolve(1)],
  ["an arrow function", () => () => 1],
  ["a class constructor", () => class {}],
  ["a URL", () => new URL("https://example.com/")],
  ["a URLSearchParams", () => new URLSearchParams("a=1")],
  ["a TextEncoder", () => new TextEncoder()],
  ["a TextDecoder", () => new TextDecoder()],
  ["a Headers", () => new Headers()],
  ["an AbortController", () => new AbortController()],
  ["an EventTarget", () => new EventTarget()],
  ["an Event", () => new Event("x")],
];

const isAccessor = (object, key) => "get" in Object.getOwnPropertyDescriptor(object, key);

describe.each(receivers)("assignment to an own accessor of %s", (label, make) => {
  test("throws TypeError when the accessor has no setter", () => {
    const receiver = make();
    Object.defineProperty(receiver, "x", {
      get: () => "from getter",
      configurable: true,
    });

    expect(() => {
      receiver.x = 1;
    }).toThrow(TypeError);
    expect(isAccessor(receiver, "x")).toBe(true);
    expect(receiver.x).toBe("from getter");
  });

  test("calls the setter once with the receiver as this", () => {
    const receiver = make();
    const calls = [];
    Object.defineProperty(receiver, "x", {
      set(value) {
        calls.push([this === receiver, value]);
      },
      configurable: true,
    });

    receiver.x = 7;

    expect(calls).toEqual([[true, 7]]);
    expect(isAccessor(receiver, "x")).toBe(true);
  });

  test("stores through a non-configurable accessor pair under a computed key", () => {
    const receiver = make();
    const key = "x";
    let stored = 10;
    let writes = 0;
    Object.defineProperty(receiver, key, {
      get: () => stored,
      set(value) {
        writes += 1;
        stored = value;
      },
    });

    receiver[key] = 20;

    expect(writes).toBe(1);
    expect(receiver[key]).toBe(20);
    expect(isAccessor(receiver, key)).toBe(true);
  });
});

describe("assignment forms on a class instance with an own accessor", () => {
  const withPair = (initial) => {
    const state = { stored: initial, writes: 0 };
    state.target = new Plain();
    Object.defineProperty(state.target, "x", {
      get: () => state.stored,
      set(value) {
        state.writes += 1;
        state.stored = value;
      },
      configurable: true,
    });
    return state;
  };

  const withGetterOnly = () => {
    const target = new Plain();
    Object.defineProperty(target, "x", { get: () => 1, configurable: true });
    return target;
  };

  test("the assignment expression evaluates to the assigned value, not the setter's result", () => {
    const target = new Plain();
    Object.defineProperty(target, "x", {
      set(value) {
        return "ignored";
      },
      configurable: true,
    });

    expect((target.x = 5)).toBe(5);
  });

  test("compound assignment writes through the setter", () => {
    const state = withPair(10);

    state.target.x += 5;

    expect(state.writes).toBe(1);
    expect(state.stored).toBe(15);
  });

  test("increment writes through the setter", () => {
    const state = withPair(10);

    state.target.x++;

    expect(state.writes).toBe(1);
    expect(state.stored).toBe(11);
  });

  test("logical assignment writes through the setter", () => {
    const state = withPair(0);

    state.target.x ||= 9;

    expect(state.writes).toBe(1);
    expect(state.stored).toBe(9);
  });

  test("compound assignment throws TypeError when the accessor has no setter", () => {
    const target = withGetterOnly();

    expect(() => {
      target.x += 5;
    }).toThrow(TypeError);
    expect(isAccessor(target, "x")).toBe(true);
    expect(target.x).toBe(1);
  });

  test("increment throws TypeError when the accessor has no setter", () => {
    const target = withGetterOnly();

    expect(() => {
      target.x++;
    }).toThrow(TypeError);
    expect(isAccessor(target, "x")).toBe(true);
  });

  test("an array destructuring target writes through the setter", () => {
    const state = withPair(0);

    [state.target.x] = [3];

    expect(state.writes).toBe(1);
    expect(state.stored).toBe(3);
  });

  test("an object destructuring target writes through the setter", () => {
    const state = withPair(0);

    ({ a: state.target.x } = { a: 4 });

    expect(state.writes).toBe(1);
    expect(state.stored).toBe(4);
  });

  test("a destructuring target throws TypeError when the accessor has no setter", () => {
    const target = withGetterOnly();

    expect(() => {
      [target.x] = [3];
    }).toThrow(TypeError);
    expect(isAccessor(target, "x")).toBe(true);
  });

  test("a for-of target writes through the setter", () => {
    const state = withPair(0);

    for (state.target.x of [1, 2]) {
      // The loop head performs the assignment.
    }

    expect(state.writes).toBe(2);
    expect(state.stored).toBe(2);
  });

  test("a string-literal key throws TypeError when the accessor has no setter", () => {
    const target = withGetterOnly();

    expect(() => {
      target["x"] = 2;
    }).toThrow(TypeError);
    expect(isAccessor(target, "x")).toBe(true);
  });

  test("an integer-like key writes through the setter", () => {
    const target = new Plain();
    const received = [];
    Object.defineProperty(target, "7", {
      set(value) {
        received.push(value);
      },
      configurable: true,
    });

    target[7] = "seven";

    expect(received).toEqual(["seven"]);
    expect(isAccessor(target, "7")).toBe(true);
  });

  test("a setter that throws propagates its error and keeps the accessor", () => {
    const target = new Plain();
    Object.defineProperty(target, "x", {
      set(value) {
        throw new RangeError("rejected");
      },
      configurable: true,
    });

    expect(() => {
      target.x = 1;
    }).toThrow(RangeError);
    expect(isAccessor(target, "x")).toBe(true);
  });

  test("a setter may replace its own property with a data property", () => {
    const target = new Plain();
    Object.defineProperty(target, "x", {
      set(value) {
        Object.defineProperty(this, "x", { value, writable: true, configurable: true });
      },
      configurable: true,
    });

    target.x = 1;
    target.x = 2;

    expect(Object.getOwnPropertyDescriptor(target, "x").value).toBe(2);
  });

  test("a bound function is called as the setter with its bound this", () => {
    const target = new Plain();
    const seen = [];
    const holder = {
      tag: "bound",
      record(value) {
        seen.push([this.tag, value]);
      },
    };
    Object.defineProperty(target, "x", { set: holder.record.bind(holder), configurable: true });

    target.x = 4;

    expect(seen).toEqual([["bound", 4]]);
  });

  test("a callable Proxy is called as the setter", () => {
    const target = new Plain();
    const received = [];
    const setter = new Proxy((value) => {
      received.push(value);
    }, {});
    Object.defineProperty(target, "x", { set: setter, configurable: true });

    target.x = 4;

    expect(received).toEqual([4]);
  });

  test("a frozen instance still writes through its setter", () => {
    const state = withPair(1);
    Object.freeze(state.target);

    state.target.x = 2;

    expect(state.writes).toBe(1);
    expect(state.stored).toBe(2);
  });

  test("Reflect.set returns false for a getter-only accessor and keeps it", () => {
    const target = withGetterOnly();

    expect(Reflect.set(target, "x", 2)).toBe(false);
    expect(isAccessor(target, "x")).toBe(true);
    expect(target.x).toBe(1);
  });
});

describe("this.x = value in a class with an own accessor on the instance", () => {
  test("a constructor write calls the setter", () => {
    const received = [];
    class Sink {
      constructor() {
        Object.defineProperty(this, "x", {
          set(value) {
            received.push(value);
          },
          configurable: true,
        });
        this.x = 5;
      }
    }

    const sink = new Sink();

    expect(received).toEqual([5]);
    expect(isAccessor(sink, "x")).toBe(true);
  });

  test("a constructor write throws TypeError when the accessor has no setter", () => {
    class ReadOnly {
      constructor() {
        Object.defineProperty(this, "x", { get: () => 1, configurable: true });
        this.x = 5;
      }
    }

    expect(() => new ReadOnly()).toThrow(TypeError);
  });

  test("a method write calls the setter", () => {
    const received = [];
    class Sink {
      write(value) {
        this.x = value;
      }
    }
    const sink = new Sink();
    Object.defineProperty(sink, "x", {
      set(value) {
        received.push(value);
      },
      configurable: true,
    });

    sink.write(1);
    sink.write(2);

    expect(received).toEqual([1, 2]);
    expect(isAccessor(sink, "x")).toBe(true);
  });

  test("a constructor write still creates a data property when there is no accessor", () => {
    class Point {
      constructor(x) {
        this.x = x;
      }
    }

    expect(Object.getOwnPropertyDescriptor(new Point(3), "x")).toEqual({
      value: 3,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });
});

describe("repeated writes to a class instance through one call site", () => {
  const writeX = (object, value) => {
    object.x = value;
  };

  test("call the setter once the data property becomes an accessor", () => {
    class Point {
      constructor() {
        this.x = 0;
      }
    }
    const point = new Point();
    const received = [];

    writeX(point, 1);
    writeX(point, 2);
    expect(point.x).toBe(2);
    Object.defineProperty(point, "x", {
      set(value) {
        received.push(value);
      },
      configurable: true,
    });
    writeX(point, 3);
    writeX(point, 4);

    expect(received).toEqual([3, 4]);
    expect(isAccessor(point, "x")).toBe(true);
  });

  test("store a value once the accessor becomes a data property", () => {
    const point = new Plain();
    const received = [];
    Object.defineProperty(point, "x", {
      set(value) {
        received.push(value);
      },
      configurable: true,
    });

    writeX(point, 1);
    writeX(point, 2);
    Object.defineProperty(point, "x", { value: 0, writable: true, configurable: true });
    writeX(point, 3);

    expect(received).toEqual([1, 2]);
    expect(point.x).toBe(3);
  });

  test("see an accessor on one instance and a data property on another", () => {
    const plainPoint = new Plain();
    const guarded = new Plain();
    const received = [];
    plainPoint.x = 0;
    Object.defineProperty(guarded, "x", {
      set(value) {
        received.push(value);
      },
      configurable: true,
    });

    writeX(plainPoint, 1);
    writeX(guarded, 2);
    writeX(plainPoint, 3);
    writeX(guarded, 4);

    expect(plainPoint.x).toBe(3);
    expect(received).toEqual([2, 4]);
  });
});

describe("built-in methods that assign to their receiver", () => {
  test("Array.prototype.push writes length through an own setter of a class instance", () => {
    const target = new Plain();
    const lengths = [];
    Object.defineProperty(target, "length", {
      get: () => 0,
      set(value) {
        lengths.push(value);
      },
      configurable: true,
    });

    Array.prototype.push.call(target, "first");

    expect(lengths).toEqual([1]);
    expect(target[0]).toBe("first");
    expect(isAccessor(target, "length")).toBe(true);
  });

  test("Array.prototype.push throws TypeError when the instance's length has no setter", () => {
    const target = new Plain();
    Object.defineProperty(target, "length", { get: () => 0, configurable: true });

    expect(() => Array.prototype.push.call(target, "first")).toThrow(TypeError);
    expect(isAccessor(target, "length")).toBe(true);
  });

  test("Array.prototype.fill throws TypeError on a getter-only element of a Map", () => {
    const target = new Map();
    Object.defineProperty(target, "0", { get: () => "kept", configurable: true });
    Object.defineProperty(target, "length", { value: 1, writable: true, configurable: true });

    expect(() => Array.prototype.fill.call(target, 5)).toThrow(TypeError);
    expect(target[0]).toBe("kept");
  });

  test("Array.prototype.fill writes an array element through its setter", () => {
    const target = [1, 2, 3];
    const received = [];
    Object.defineProperty(target, "1", {
      get: () => "kept",
      set(value) {
        received.push(value);
      },
      configurable: true,
    });

    target.fill(9);

    expect(received).toEqual([9]);
    expect(target[0]).toBe(9);
    expect(target[1]).toBe("kept");
    expect(target[2]).toBe(9);
  });
});
