/*---
description: Reflect.construct accepts function declarations and expressions as the target — they own a [[Construct]] slot per spec
features: [Reflect, compat-function]
---*/

describe("Reflect.construct on function-keyword targets", () => {
  test("function declaration target produces an instance whose [[Prototype]] is target.prototype", () => {
    function Foo() {
      this.x = 1;
    }
    const obj = Reflect.construct(Foo, []);
    expect(obj.x).toBe(1);
    expect(Object.getPrototypeOf(obj)).toBe(Foo.prototype);
    expect(obj instanceof Foo).toBe(true);
  });

  test("function expression target produces an instance with arguments forwarded", () => {
    const Foo = function (x, y) {
      this.sum = x + y;
    };
    const obj = Reflect.construct(Foo, [3, 4]);
    expect(obj.sum).toBe(7);
    expect(obj instanceof Foo).toBe(true);
  });

  test("named function expression target works the same as a declaration", () => {
    const Foo = function NamedFoo(x) {
      this.value = x;
    };
    const obj = Reflect.construct(Foo, [42]);
    expect(obj.value).toBe(42);
    expect(obj instanceof Foo).toBe(true);
  });

  test("constructor body sees `this` bound to the freshly created receiver", () => {
    function Capture() {
      this.captured = this;
    }
    const obj = Reflect.construct(Capture, []);
    expect(obj.captured).toBe(obj);
  });

  test("explicit object return replaces the constructed receiver", () => {
    function Replace() {
      this.fromBody = true;
      return { replaced: true };
    }
    const obj = Reflect.construct(Replace, []);
    expect(obj.replaced).toBe(true);
    expect(obj.fromBody).toBeUndefined();
  });

  test("explicit primitive return is ignored — receiver wins", () => {
    function ReturnPrimitive() {
      this.kept = true;
      return 42;
    }
    const obj = Reflect.construct(ReturnPrimitive, []);
    expect(obj.kept).toBe(true);
  });

  test("newTarget controls the instance [[Prototype]]", () => {
    function Foo() {}
    function Bar() {}
    const obj = Reflect.construct(Foo, [], Bar);
    expect(Object.getPrototypeOf(obj)).toBe(Bar.prototype);
    expect(obj instanceof Bar).toBe(true);
    expect(obj instanceof Foo).toBe(false);
  });

  test("when newTarget.prototype is non-object, the instance falls back to Object.prototype", () => {
    function Foo() {
      this.x = 1;
    }
    function Bar() {}
    Bar.prototype = null;
    const obj = Reflect.construct(Foo, [], Bar);
    expect(obj.x).toBe(1);
    expect(Object.getPrototypeOf(obj)).toBe(Object.prototype);
  });

  test("instanceof against newTarget is observable from inside the constructor body", () => {
    function Bar() {}
    function Base() {
      this.isBar = this instanceof Bar;
    }
    const obj = Reflect.construct(Base, [], Bar);
    expect(obj.isBar).toBe(true);
  });

  test("methods inherited from newTarget.prototype are reachable during construction", () => {
    function Target() {
      this.greeting = this.greet();
    }
    function NewTarget() {}
    NewTarget.prototype.greet = function () {
      return "hello from newTarget";
    };
    const obj = Reflect.construct(Target, [], NewTarget);
    expect(obj.greeting).toBe("hello from newTarget");
  });

  test("matches `new` semantics for function declaration targets", () => {
    function Foo(x) {
      this.x = x;
    }
    const viaNew = new Foo(7);
    const viaReflect = Reflect.construct(Foo, [7]);
    expect(Object.getPrototypeOf(viaReflect)).toBe(Object.getPrototypeOf(viaNew));
    expect(viaReflect.x).toBe(viaNew.x);
  });
});

describe("Reflect.construct on bound function-keyword targets", () => {
  test("bound function declaration target inherits constructibility from the underlying function", () => {
    function Foo() {
      this.x = 1;
    }
    const Bound = Foo.bind(null);
    const obj = Reflect.construct(Bound, []);
    expect(obj.x).toBe(1);
    expect(Object.getPrototypeOf(obj)).toBe(Foo.prototype);
    expect(obj instanceof Foo).toBe(true);
  });

  test("bound function merges bound and call arguments in order", () => {
    function Pair(a, b) {
      this.a = a;
      this.b = b;
    }
    const Bound = Pair.bind(null, 1);
    const obj = Reflect.construct(Bound, [2]);
    expect(obj.a).toBe(1);
    expect(obj.b).toBe(2);
  });

  test("bound function ignores the bound `this` — receiver is the constructed instance", () => {
    function Capture() {
      this.captured = this;
    }
    const sentinel = { sentinel: true };
    const Bound = Capture.bind(sentinel);
    const obj = Reflect.construct(Bound, []);
    expect(obj.captured).toBe(obj);
    expect(obj.captured).not.toBe(sentinel);
  });

  test("doubly-bound function still routes through the original target", () => {
    function Triple(a, b, c) {
      this.sum = a + b + c;
    }
    const Once = Triple.bind(null, 1);
    const Twice = Once.bind(null, 2);
    const obj = Reflect.construct(Twice, [3]);
    expect(obj.sum).toBe(6);
    expect(Object.getPrototypeOf(obj)).toBe(Triple.prototype);
  });

  test("bound function with explicit newTarget honors newTarget.prototype", () => {
    function Foo() {}
    function NewTarget() {}
    const Bound = Foo.bind(null);
    const obj = Reflect.construct(Bound, [], NewTarget);
    expect(Object.getPrototypeOf(obj)).toBe(NewTarget.prototype);
  });

  test("bound non-constructable function (bound arrow) still throws TypeError", () => {
    const arrow = () => {};
    const Bound = arrow.bind(null);
    expect(() => Reflect.construct(Bound, [])).toThrow(TypeError);
  });
});

describe("Reflect.construct rejects non-constructable function forms", () => {
  test("async function declaration target throws TypeError", () => {
    async function asyncFn() {}
    expect(() => Reflect.construct(asyncFn, [])).toThrow(TypeError);
  });

  test("generator function declaration target throws TypeError", () => {
    function* gen() {}
    expect(() => Reflect.construct(gen, [])).toThrow(TypeError);
  });

  test("async generator function declaration target throws TypeError", () => {
    async function* asyncGen() {}
    expect(() => Reflect.construct(asyncGen, [])).toThrow(TypeError);
  });

  test("function declaration is rejected as newTarget when not constructable (sanity: declarations always are)", () => {
    function Foo() {}
    async function asyncFn() {}
    expect(() => Reflect.construct(Foo, [], asyncFn)).toThrow(TypeError);
  });
});
