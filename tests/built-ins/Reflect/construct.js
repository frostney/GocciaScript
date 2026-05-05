/*---
description: Reflect.construct
features: [Reflect]
---*/

describe("Reflect.construct", () => {
  test("creates a new instance of a class", () => {
    class Foo {
      constructor(x) {
        this.x = x;
      }
    }
    const obj = Reflect.construct(Foo, [42]);
    expect(obj.x).toBe(42);
    expect(obj instanceof Foo).toBe(true);
  });

  test("works with empty arguments list", () => {
    class Bar {
      constructor() {
        this.value = "default";
      }
    }
    const obj = Reflect.construct(Bar, []);
    expect(obj.value).toBe("default");
  });

  test("uses newTarget to set prototype", () => {
    class Foo {
      constructor(x) {
        this.x = x;
      }
    }
    class Bar {
      getX() {
        return this.x;
      }
    }
    const obj = Reflect.construct(Foo, [10], Bar);
    expect(obj.x).toBe(10);
    expect(obj instanceof Bar).toBe(true);
  });

  test("throws TypeError if target is not a constructor", () => {
    expect(() => Reflect.construct(() => {}, [])).toThrow(TypeError);
    expect(() => Reflect.construct(42, [])).toThrow(TypeError);
  });

  test("throws TypeError if newTarget is not a constructor", () => {
    class Foo {}
    expect(() => Reflect.construct(Foo, [], 42)).toThrow(TypeError);
    expect(() => Reflect.construct(Foo, [], () => {})).toThrow(TypeError);
  });

  test("works with array-like objects", () => {
    class Point {
      constructor(x, y) {
        this.x = x;
        this.y = y;
      }
    }
    const arrayLike = { 0: 3, 1: 4, length: 2 };
    const point = Reflect.construct(Point, arrayLike);
    expect(point.x).toBe(3);
    expect(point.y).toBe(4);
    expect(point instanceof Point).toBe(true);
  });

  test("works with array-like object with zero length", () => {
    class Empty {
      constructor() {
        this.value = "constructed";
      }
    }
    const obj = Reflect.construct(Empty, { length: 0 });
    expect(obj.value).toBe("constructed");
  });

  test("works with array-like object with missing indices", () => {
    class Pair {
      constructor(a, b) {
        this.a = a;
        this.b = b;
      }
    }
    const arrayLike = { 0: "first", length: 2 };
    const pair = Reflect.construct(Pair, arrayLike);
    expect(pair.a).toBe("first");
    expect(pair.b).toBe(undefined);
  });

  test("throws TypeError if argumentsList is not an object", () => {
    class Foo {}
    expect(() => Reflect.construct(Foo, "not-array")).toThrow(TypeError);
    expect(() => Reflect.construct(Foo, 42)).toThrow(TypeError);
    expect(() => Reflect.construct(Foo, true)).toThrow(TypeError);
  });

  test("newTarget prototype is visible during constructor execution", () => {
    class Bar {}
    class Foo {
      constructor() {
        this.protoWasCorrect = (Object.getPrototypeOf(this) === Bar.prototype);
      }
    }

    const obj = Reflect.construct(Foo, [], Bar);
    expect(obj instanceof Bar).toBe(true);
    expect(obj.protoWasCorrect).toBe(true);
  });

  test("newTarget prototype is visible for instanceof during construction", () => {
    class Bar {}
    class Base {
      constructor() {
        this.isBar = (this instanceof Bar);
      }
    }

    const obj = Reflect.construct(Base, [], Bar);
    expect(obj.isBar).toBe(true);
  });

  test("constructor can call inherited methods from newTarget prototype", () => {
    class Target {
      constructor() {
        this.greeting = this.greet();
      }
    }
    class NewTarget {
      greet() {
        return "hello from NewTarget";
      }
    }

    const obj = Reflect.construct(Target, [], NewTarget);
    expect(obj.greeting).toBe("hello from NewTarget");
  });

  test("newTarget same as target preserves default behavior", () => {
    class Foo {
      constructor() {
        this.isFoo = (this instanceof Foo);
      }
    }

    const obj = Reflect.construct(Foo, [], Foo);
    expect(obj instanceof Foo).toBe(true);
    expect(obj.isFoo).toBe(true);
  });

  test("rejects an object shorthand method (no [[Construct]] slot)", () => {
    const obj = { m() {} };
    expect(() => Reflect.construct(obj.m, [])).toThrow(TypeError);
  });

  test("rejects a class instance method reached via prototype (no [[Construct]] slot)", () => {
    class C {
      m() {}
    }
    expect(() => Reflect.construct(C.prototype.m, [])).toThrow(TypeError);
  });

  test("rejects an arrow function used as newTarget", () => {
    class Foo {}
    expect(() => Reflect.construct(Foo, [], () => {})).toThrow(TypeError);
  });

  test("constructs through a bound class — bound `this` is ignored, instance is fresh", () => {
    class Counter {
      constructor(start) {
        this.value = start;
      }
      next() {
        this.value += 1;
        return this.value;
      }
    }
    const Bound = Counter.bind(null, 10);
    const obj = Reflect.construct(Bound, []);
    expect(obj.value).toBe(10);
    expect(obj.next()).toBe(11);
    expect(obj instanceof Counter).toBe(true);
  });

  test("bound class with explicit newTarget uses newTarget.prototype on the instance", () => {
    class Base {
      constructor() {
        this.x = 1;
      }
    }
    class Other {}
    const Bound = Base.bind(null);
    const obj = Reflect.construct(Bound, [], Other);
    expect(obj.x).toBe(1);
    expect(Object.getPrototypeOf(obj)).toBe(Other.prototype);
  });

  test("bound non-constructable target (bound arrow) still throws", () => {
    const Bound = (() => {}).bind(null);
    expect(() => Reflect.construct(Bound, [])).toThrow(TypeError);
  });

  test("Proxy over a class invokes the construct trap", () => {
    class Foo {
      constructor(x) {
        this.x = x;
      }
    }
    let trapCalled = false;
    const proxy = new Proxy(Foo, {
      construct(target, args, newTarget) {
        trapCalled = true;
        return Reflect.construct(target, args, newTarget);
      },
    });
    const obj = Reflect.construct(proxy, [99]);
    expect(trapCalled).toBe(true);
    expect(obj.x).toBe(99);
    expect(obj instanceof Foo).toBe(true);
  });

  test("Proxy over a class with no construct trap falls through to the target", () => {
    class Foo {
      constructor(x) {
        this.x = x;
      }
    }
    const proxy = new Proxy(Foo, {});
    const obj = Reflect.construct(proxy, [7]);
    expect(obj.x).toBe(7);
    expect(obj instanceof Foo).toBe(true);
  });

  test("Proxy construct trap receives the explicit newTarget supplied to Reflect.construct", () => {
    class Foo {}
    class Other {}
    let receivedNewTarget;
    const proxy = new Proxy(Foo, {
      construct(target, args, newTarget) {
        receivedNewTarget = newTarget;
        return Reflect.construct(target, args, newTarget);
      },
    });
    Reflect.construct(proxy, [], Other);
    expect(receivedNewTarget).toBe(Other);
  });

  test("Proxy construct trap receives the proxy as default newTarget when none is supplied", () => {
    class Foo {}
    let receivedNewTarget;
    const proxy = new Proxy(Foo, {
      construct(target, args, newTarget) {
        receivedNewTarget = newTarget;
        return Reflect.construct(target, args, newTarget);
      },
    });
    Reflect.construct(proxy, []);
    expect(receivedNewTarget).toBe(proxy);
  });

  test("nested proxy forwards newTarget through the outer fallback to the inner construct trap", () => {
    class Foo {}
    class Other {}
    let inner;
    const innerProxy = new Proxy(Foo, {
      construct(target, args, newTarget) {
        inner = newTarget;
        return Reflect.construct(target, args, newTarget);
      },
    });
    const outerProxy = new Proxy(innerProxy, {});
    Reflect.construct(outerProxy, [], Other);
    expect(inner).toBe(Other);
  });
});
