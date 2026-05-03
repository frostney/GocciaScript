/*---
description: `new` on a function-keyword constructor allocates a fresh object, binds `this`, and applies the spec [[Construct]] return rules
features: [compat-function]
---*/

test("typeof new Foo() is 'object' on an empty body", () => {
  function Foo() {}
  expect(typeof new Foo()).toBe("object");
});

test("new Foo() instanceof Foo is true", () => {
  function Foo() {}
  expect(new Foo() instanceof Foo).toBe(true);
});

test("new Foo() instance [[Prototype]] is Foo.prototype", () => {
  function Foo() {}
  const instance = new Foo();
  expect(Object.getPrototypeOf(instance)).toBe(Foo.prototype);
});

test("methods set on Foo.prototype are reachable on instances", () => {
  function Animal() {}
  Animal.prototype.speak = function () {
    return "generic sound";
  };
  expect(new Animal().speak()).toBe("generic sound");
});

test("constructor body sees `this` bound to the new instance", () => {
  function Box(value) {
    this.value = value;
  }
  const b = new Box("x");
  expect(b.value).toBe("x");
});

test("constructor arguments forward to the body", () => {
  function Pair(a, b) {
    this.a = a;
    this.b = b;
  }
  const p = new Pair(1, 2);
  expect(p.a).toBe(1);
  expect(p.b).toBe(2);
});

test("constructor body running once per `new` call", () => {
  let calls = 0;
  function Counter() {
    calls += 1;
  }
  new Counter();
  new Counter();
  new Counter();
  expect(calls).toBe(3);
});

test("explicit object return replaces the constructed receiver", () => {
  function Replace() {
    this.fromBody = true;
    return { replaced: true };
  }
  const r = new Replace();
  expect(r.replaced).toBe(true);
  expect(r.fromBody).toBeUndefined();
});

test("explicit primitive return is ignored — receiver wins", () => {
  function ReturnPrimitive() {
    this.kept = true;
    return 42;
  }
  const r = new ReturnPrimitive();
  expect(r.kept).toBe(true);
});

test("explicit undefined return is ignored — receiver wins", () => {
  function ReturnUndefined() {
    this.kept = true;
    return undefined;
  }
  const r = new ReturnUndefined();
  expect(r.kept).toBe(true);
});

test("explicit null return is ignored — receiver wins", () => {
  function ReturnNull() {
    this.kept = true;
    return null;
  }
  const r = new ReturnNull();
  expect(r.kept).toBe(true);
});

test("explicit array return replaces the constructed receiver (arrays are objects)", () => {
  function ReturnArray() {
    this.discarded = true;
    return [1, 2, 3];
  }
  const r = new ReturnArray();
  expect(Array.isArray(r)).toBe(true);
  expect(r.length).toBe(3);
  expect(r.discarded).toBeUndefined();
});

test("instances are distinct objects across calls", () => {
  function Foo() {}
  expect(new Foo()).not.toBe(new Foo());
});

test("reassigning Foo.prototype after construction does not change existing instances", () => {
  function Foo() {}
  const before = new Foo();
  const oldProto = Foo.prototype;
  Foo.prototype = { replaced: true };
  expect(Object.getPrototypeOf(before)).toBe(oldProto);
});

test("new Foo() with spread arguments forwards them in order", () => {
  function Triple(a, b, c) {
    this.sum = a + b + c;
  }
  const args = [10, 20, 30];
  const t = new Triple(...args);
  expect(t.sum).toBe(60);
});

test("`new` on an arrow function throws TypeError", () => {
  const arrow = () => {};
  expect(() => new arrow()).toThrow(TypeError);
});

test("`new` on an async function throws TypeError", () => {
  async function asyncFn() {}
  expect(() => new asyncFn()).toThrow(TypeError);
});

test("`new` on a generator function throws TypeError", () => {
  function* gen() {}
  expect(() => new gen()).toThrow(TypeError);
});

test("`new` on a concise method throws TypeError (no own prototype)", () => {
  const obj = {
    method() {},
  };
  expect(() => new obj.method()).toThrow(TypeError);
});

test("instance receives properties added by the body before any explicit return", () => {
  function Sequenced() {
    this.first = 1;
    this.second = 2;
  }
  const s = new Sequenced();
  expect(s.first).toBe(1);
  expect(s.second).toBe(2);
});

test("Foo.prototype.constructor is reachable from the instance via the prototype chain", () => {
  function Foo() {}
  const instance = new Foo();
  expect(instance.constructor).toBe(Foo);
});

test("nested `new` inside a constructor body works", () => {
  function Inner(value) {
    this.value = value;
  }
  function Outer(value) {
    this.inner = new Inner(value);
  }
  const o = new Outer("nested");
  expect(o.inner.value).toBe("nested");
});

test("when Foo.prototype is reassigned to a non-object, instances fall back to Object.prototype", () => {
  function Foo() {}
  Foo.prototype = null;
  const instance = new Foo();
  expect(Object.getPrototypeOf(instance)).toBe(Object.prototype);
});
