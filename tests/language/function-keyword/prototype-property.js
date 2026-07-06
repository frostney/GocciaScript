/*---
description: Function declarations and expressions own a `prototype` data property per ES2026 §10.2.5 MakeConstructor
features: [compat-function]
---*/

test("function declaration has own prototype property", () => {
  function f() {}
  expect(typeof f.prototype).toBe("object");
  expect(f.prototype).not.toBeNull();
});

test("function declaration prototype.constructor back-references the function", () => {
  function f() {}
  expect(f.prototype.constructor).toBe(f);
});

test("function expression has own prototype property", () => {
  const f = function () {};
  expect(typeof f.prototype).toBe("object");
  expect(f.prototype.constructor).toBe(f);
});

test("named function expression has own prototype property", () => {
  const f = function named() {};
  expect(typeof f.prototype).toBe("object");
  expect(f.prototype.constructor).toBe(f);
});

test("each function has its own distinct prototype object", () => {
  function f() {}
  function g() {}
  expect(f.prototype).not.toBe(g.prototype);
});

test("prototype identity is stable across reads", () => {
  function f() {}
  expect(f.prototype).toBe(f.prototype);
});

test("function prototype property descriptor", () => {
  function f() {}
  const desc = Object.getOwnPropertyDescriptor(f, "prototype");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("function prototype.constructor property descriptor", () => {
  function f() {}
  const desc = Object.getOwnPropertyDescriptor(f.prototype, "constructor");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});

test("function prototype is writable: reassignment sticks", () => {
  function f() {}
  const replacement = { custom: 42 };
  f.prototype = replacement;
  expect(f.prototype).toBe(replacement);
  expect(f.prototype.custom).toBe(42);
});

test("hasOwnProperty('prototype') is true on function declarations", () => {
  function f() {}
  expect(Object.prototype.hasOwnProperty.call(f, "prototype")).toBe(true);
});

test("function superclass constructor runs once from explicit super", () => {
  let calls = 0;
  function Base(value) {
    calls += 1;
    this.value = value;
  }

  class Derived extends Base {
    constructor(value) {
      super("super-" + value);
      this.done = true;
    }
  }

  const derived = new Derived("arg");
  expect(calls).toBe(1);
  expect(derived.value).toBe("super-arg");
  expect(derived.done).toBe(true);

  class ImplicitDerived extends Base {}
  const implicit = new ImplicitDerived("implicit");
  expect(calls).toBe(2);
  expect(implicit.value).toBe("implicit");
});

test("function superclass returned object becomes the constructed receiver", () => {
  let calls = 0;
  function Base(value) {
    calls += 1;
    return { value, fromBase: true };
  }

  class ExplicitDerived extends Base {
    constructor(value) {
      const returned = super("explicit-" + value);
      this.superReturnedThis = returned === this;
      this.afterSuper = true;
      return this;
    }
  }

  const explicit = new ExplicitDerived("arg");
  expect(explicit.value).toBe("explicit-arg");
  expect(explicit.fromBase).toBe(true);
  expect(explicit.superReturnedThis).toBe(true);
  expect(explicit.afterSuper).toBe(true);

  class ImplicitDerived extends Base {
    field = "field";
  }
  const implicit = new ImplicitDerived("implicit");
  expect(implicit.value).toBe("implicit");
  expect(implicit.fromBase).toBe(true);
  expect(implicit.field).toBe("field");
  expect(calls).toBe(2);
});

test("function superclass replacement receives private fields", () => {
  let derivedPrototype;

  function Base() {
    const replacement = Object.create(derivedPrototype);
    replacement.fromBase = true;
    return replacement;
  }

  class Derived extends Base {
    #value = 42;

    getValue() {
      return this.#value;
    }
  }

  derivedPrototype = Derived.prototype;
  const derived = new Derived();
  expect(derived.fromBase).toBe(true);
  expect(derived.getValue()).toBe(42);
});

test("function.prototype's [[Prototype]] is Object.prototype", () => {
  function f() {}
  expect(Object.getPrototypeOf(f.prototype)).toBe(Object.prototype);
});

test("async function object's [[Prototype]] is non-callable AsyncFunction.prototype", () => {
  async function f() {}
  const asyncFunctionPrototype = Object.getPrototypeOf(f);
  expect(typeof asyncFunctionPrototype).toBe("object");
  expect(asyncFunctionPrototype).not.toBe(Function.prototype);
  expect(Object.getPrototypeOf(asyncFunctionPrototype)).toBe(Function.prototype);
  expect(Object.prototype.hasOwnProperty.call(asyncFunctionPrototype, "length")).toBe(false);
  expect(Object.prototype.hasOwnProperty.call(asyncFunctionPrototype, "name")).toBe(false);
  expect(() => asyncFunctionPrototype()).toThrow(TypeError);
});

test("generator function object's [[Prototype]] is non-callable GeneratorFunction.prototype", () => {
  function* g() {}
  const generatorFunctionPrototype = Object.getPrototypeOf(g);
  expect(typeof generatorFunctionPrototype).toBe("object");
  expect(generatorFunctionPrototype).not.toBe(Function.prototype);
  expect(Object.getPrototypeOf(generatorFunctionPrototype)).toBe(Function.prototype);
  expect(Object.prototype.hasOwnProperty.call(generatorFunctionPrototype, "length")).toBe(false);
  expect(Object.prototype.hasOwnProperty.call(generatorFunctionPrototype, "name")).toBe(false);
  expect(() => generatorFunctionPrototype()).toThrow(TypeError);
});

test("async generator function object's [[Prototype]] is non-callable AsyncGeneratorFunction.prototype", () => {
  async function* g() {}
  const asyncGeneratorFunctionPrototype = Object.getPrototypeOf(g);
  expect(typeof asyncGeneratorFunctionPrototype).toBe("object");
  expect(asyncGeneratorFunctionPrototype).not.toBe(Function.prototype);
  expect(Object.getPrototypeOf(asyncGeneratorFunctionPrototype)).toBe(Function.prototype);
  expect(Object.prototype.hasOwnProperty.call(asyncGeneratorFunctionPrototype, "length")).toBe(false);
  expect(Object.prototype.hasOwnProperty.call(asyncGeneratorFunctionPrototype, "name")).toBe(false);
  expect(() => asyncGeneratorFunctionPrototype()).toThrow(TypeError);
});

test("async and generator function prototype constructors are their own intrinsics", () => {
  async function af() {}
  function* gf() {}
  async function* agf() {}

  const asyncFunctionPrototype = Object.getPrototypeOf(af);
  const generatorFunctionPrototype = Object.getPrototypeOf(gf);
  const asyncGeneratorFunctionPrototype = Object.getPrototypeOf(agf);

  const asyncFunctionConstructor = asyncFunctionPrototype.constructor;
  const generatorFunctionConstructor = generatorFunctionPrototype.constructor;
  const asyncGeneratorFunctionConstructor = asyncGeneratorFunctionPrototype.constructor;

  expect(asyncFunctionConstructor).not.toBe(Function);
  expect(generatorFunctionConstructor).not.toBe(Function);
  expect(asyncGeneratorFunctionConstructor).not.toBe(Function);

  expect(typeof asyncFunctionConstructor).toBe("function");
  expect(typeof generatorFunctionConstructor).toBe("function");
  expect(typeof asyncGeneratorFunctionConstructor).toBe("function");

  expect(asyncFunctionConstructor.name).toBe("AsyncFunction");
  expect(generatorFunctionConstructor.name).toBe("GeneratorFunction");
  expect(asyncGeneratorFunctionConstructor.name).toBe("AsyncGeneratorFunction");

  expect(asyncFunctionConstructor.length).toBe(1);
  expect(generatorFunctionConstructor.length).toBe(1);
  expect(asyncGeneratorFunctionConstructor.length).toBe(1);

  expect(asyncFunctionConstructor.prototype).toBe(asyncFunctionPrototype);
  expect(generatorFunctionConstructor.prototype).toBe(generatorFunctionPrototype);
  expect(asyncGeneratorFunctionConstructor.prototype).toBe(asyncGeneratorFunctionPrototype);

  expect(Object.getPrototypeOf(asyncFunctionConstructor)).toBe(Function);
  expect(Object.getPrototypeOf(generatorFunctionConstructor)).toBe(Function);
  expect(Object.getPrototypeOf(asyncGeneratorFunctionConstructor)).toBe(Function);
});

test("generator function.prototype's [[Prototype]] is GeneratorFunction.prototype.prototype", () => {
  function* g() {}
  const generatorFunctionPrototype = Object.getPrototypeOf(g);
  expect(Object.getPrototypeOf(g.prototype)).toBe(generatorFunctionPrototype.prototype);
  expect(Object.getPrototypeOf(generatorFunctionPrototype.prototype)).toBe(
    Object.getPrototypeOf(Object.getPrototypeOf([][Symbol.iterator]()))
  );
});

test("async generator function.prototype's [[Prototype]] is AsyncGeneratorFunction.prototype.prototype", () => {
  async function* g() {}
  const asyncGeneratorFunctionPrototype = Object.getPrototypeOf(g);
  const asyncGenerator = g();
  expect(Object.getPrototypeOf(g.prototype)).toBe(asyncGeneratorFunctionPrototype.prototype);
  expect(Object.getPrototypeOf(asyncGenerator)).toBe(g.prototype);
  expect(Object.getPrototypeOf(Object.getPrototypeOf(asyncGenerator))).toBe(
    asyncGeneratorFunctionPrototype.prototype
  );
});

test("methods added to prototype are visible to instances via [[Prototype]] chain", () => {
  function Animal() {}
  Animal.prototype.speak = function () {
    return "generic sound";
  };

  const dog = new Animal();
  expect(dog.speak()).toBe("generic sound");
  expect(Object.getPrototypeOf(dog)).toBe(Animal.prototype);
});

test("constructor return primitives are ignored for ordinary functions", () => {
  function Box(value) {
    this.value = value;
    return 42;
  }

  const box = new Box("ok");
  expect(box.value).toBe("ok");
  expect(Object.getPrototypeOf(box)).toBe(Box.prototype);
});

test("constructor return objects replace the default instance", () => {
  const replacement = { value: "replacement" };
  function Box() {
    this.value = "ignored";
    return replacement;
  }

  expect(new Box()).toBe(replacement);
  expect(replacement.value).toBe("replacement");
});

test("bound ordinary functions remain constructable", () => {
  function Pair(a, b) {
    this.a = a;
    this.b = b;
    this.boundThisIgnored = this.ignored;
  }

  const BoundPair = Pair.bind({ ignored: true }, 1);
  const pair = new BoundPair(2);

  expect(pair.a).toBe(1);
  expect(pair.b).toBe(2);
  expect(pair.boundThisIgnored).toBeUndefined();
  expect(Object.getPrototypeOf(pair)).toBe(Pair.prototype);
});

test("generator function declaration has own prototype property", () => {
  function* g() {
    yield 1;
  }
  expect(typeof g.prototype).toBe("object");
  expect(g.prototype).not.toBeNull();
});

test("generator function expression has own prototype property", () => {
  const g = function* () {
    yield 1;
  };
  expect(typeof g.prototype).toBe("object");
});

// Per ES2026 §27.5.1.1, a generator's `prototype.constructor` is INHERITED from
// %Generator% and points at %GeneratorFunction.prototype%, not the specific
// generator function.  The own prototype object itself has no `constructor` and
// must not back-reference the function.
test("generator prototype has no own constructor and does not point back at the function", () => {
  function* g() {}
  expect(Object.prototype.hasOwnProperty.call(g.prototype, "constructor")).toBe(false);
  expect(g.prototype.constructor).not.toBe(g);
});

test("generator function expression: prototype has no own constructor back-reference", () => {
  const g = function* () {};
  expect(Object.prototype.hasOwnProperty.call(g.prototype, "constructor")).toBe(false);
  expect(g.prototype.constructor).not.toBe(g);
});

test("generator function prototype is writable and non-configurable", () => {
  function* g() {}
  const desc = Object.getOwnPropertyDescriptor(g, "prototype");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("generator prototype assignment updates the own prototype property", () => {
  function* g() {}
  const replacement = {};
  g.prototype = replacement;
  expect(g.prototype).toBe(replacement);
});

test("async function declaration does NOT have own prototype property", () => {
  async function f() {}
  expect(f.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(f, "prototype")).toBe(false);
});

test("async function expression does NOT have own prototype property", () => {
  const f = async function () {};
  expect(f.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(f, "prototype")).toBe(false);
});

test("async generator declaration has own prototype property", () => {
  async function* g() {
    yield 1;
  }
  expect(typeof g.prototype).toBe("object");
  expect(g.prototype).not.toBeNull();
});

test("async generator expression has own prototype property", () => {
  const g = async function* () {
    yield 1;
  };
  expect(typeof g.prototype).toBe("object");
});

// Same MakeConstructor exclusion as generators: §15.6 async generators inherit
// `constructor` from %AsyncGeneratorFunction.prototype.prototype% rather than
// owning a back-reference to the specific function.
test("async generator prototype has no own constructor and does not point back at the function", () => {
  async function* g() {}
  expect(Object.prototype.hasOwnProperty.call(g.prototype, "constructor")).toBe(false);
  expect(g.prototype.constructor).not.toBe(g);
});

test("async generator expression: prototype has no own constructor back-reference", () => {
  const g = async function* () {};
  expect(Object.prototype.hasOwnProperty.call(g.prototype, "constructor")).toBe(false);
  expect(g.prototype.constructor).not.toBe(g);
});

test("async generator declaration prototype descriptor matches generator (writable, non-configurable)", () => {
  async function* g() {}
  const desc = Object.getOwnPropertyDescriptor(g, "prototype");
  expect(typeof desc.value).toBe("object");
  expect(desc.value).not.toBeNull();
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("async generator expression prototype descriptor matches generator (writable, non-configurable)", () => {
  const g = async function* () {};
  const desc = Object.getOwnPropertyDescriptor(g, "prototype");
  expect(typeof desc.value).toBe("object");
  expect(desc.value).not.toBeNull();
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("async generator prototype assignment updates the own prototype property", () => {
  async function* g() {}
  const replacement = {};
  g.prototype = replacement;
  expect(g.prototype).toBe(replacement);
});

const expectWritableNonEnumerableConfigurableMethod = (descriptor) => {
  expect(descriptor).toBeDefined();
  if (descriptor !== undefined) {
    expect(typeof descriptor.value).toBe("function");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  }
};

test("generator prototype exposes inherited next return and throw methods", () => {
  function* values() {
    yield 1;
  }

  const iterator = values();
  const generatorPrototype = Object.getPrototypeOf(values.prototype);
  expect(Object.getPrototypeOf(Object.getPrototypeOf(iterator))).toBe(generatorPrototype);
  expect(Object.getOwnPropertyDescriptor(iterator, "next")).toBeUndefined();
  expect(Object.getOwnPropertyDescriptor(Object.getPrototypeOf(iterator), "next")).toBeUndefined();

  expectWritableNonEnumerableConfigurableMethod(
    Object.getOwnPropertyDescriptor(generatorPrototype, "next"),
  );
  expectWritableNonEnumerableConfigurableMethod(
    Object.getOwnPropertyDescriptor(generatorPrototype, "return"),
  );
  expectWritableNonEnumerableConfigurableMethod(
    Object.getOwnPropertyDescriptor(generatorPrototype, "throw"),
  );
});

test("generator next and return create ordinary iterator result objects", () => {
  function* values() {
    yield 1;
  }

  const iterator = values();
  const nextResult = iterator.next();
  const returnResult = iterator.return(2);

  expect(Object.getPrototypeOf(nextResult)).toBe(Object.prototype);
  expect(Object.getOwnPropertyDescriptor(nextResult, "value")).toEqual({
    value: 1,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(Object.getOwnPropertyDescriptor(nextResult, "done")).toEqual({
    value: false,
    writable: true,
    enumerable: true,
    configurable: true,
  });

  expect(Object.getPrototypeOf(returnResult)).toBe(Object.prototype);
  expect(Object.getOwnPropertyDescriptor(returnResult, "value")).toEqual({
    value: 2,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(Object.getOwnPropertyDescriptor(returnResult, "done")).toEqual({
    value: true,
    writable: true,
    enumerable: true,
    configurable: true,
  });
});

test("async generator prototype exposes inherited next return and throw methods", () => {
  async function* values() {
    yield 1;
  }

  const iterator = values();
  const asyncGeneratorPrototype = Object.getPrototypeOf(values.prototype);
  expect(Object.getPrototypeOf(Object.getPrototypeOf(iterator))).toBe(asyncGeneratorPrototype);
  expect(Object.getOwnPropertyDescriptor(iterator, "next")).toBeUndefined();
  expect(Object.getOwnPropertyDescriptor(Object.getPrototypeOf(iterator), "next")).toBeUndefined();

  expectWritableNonEnumerableConfigurableMethod(
    Object.getOwnPropertyDescriptor(asyncGeneratorPrototype, "next"),
  );
  expectWritableNonEnumerableConfigurableMethod(
    Object.getOwnPropertyDescriptor(asyncGeneratorPrototype, "return"),
  );
  expectWritableNonEnumerableConfigurableMethod(
    Object.getOwnPropertyDescriptor(asyncGeneratorPrototype, "throw"),
  );
});

test("async generator next and return create ordinary iterator result objects", async () => {
  async function* values() {
    yield 1;
  }

  const iterator = values();
  const nextResult = await iterator.next();
  const returnResult = await iterator.return(2);

  expect(Object.getPrototypeOf(nextResult)).toBe(Object.prototype);
  expect(Object.getOwnPropertyDescriptor(nextResult, "value")).toEqual({
    value: 1,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(Object.getOwnPropertyDescriptor(nextResult, "done")).toEqual({
    value: false,
    writable: true,
    enumerable: true,
    configurable: true,
  });

  expect(Object.getPrototypeOf(returnResult)).toBe(Object.prototype);
  expect(Object.getOwnPropertyDescriptor(returnResult, "value")).toEqual({
    value: 2,
    writable: true,
    enumerable: true,
    configurable: true,
  });
  expect(Object.getOwnPropertyDescriptor(returnResult, "done")).toEqual({
    value: true,
    enumerable: true,
    writable: true,
    configurable: true,
  });
});

test("async generator return awaits fulfilled and rejected promise values", async () => {
  async function* empty() {}
  async function* values() {
    yield 1;
  }

  await expect(empty().return(Promise.resolve("done"))).resolves.toEqual({
    value: "done",
    done: true,
  });
  await expect(empty().return(Promise.reject("boom"))).rejects.toBe("boom");

  const iterator = values();
  await expect(iterator.next()).resolves.toEqual({ value: 1, done: false });
  await expect(iterator.return(Promise.resolve(2))).resolves.toEqual({
    value: 2,
    done: true,
  });
});

test("async generator return resolves promised value once before finally", async () => {
  const events = [];
  const returnedValue = Promise.resolve("done");
  Object.defineProperty(returnedValue, "constructor", {
    get() {
      events.push("constructor");
      return Promise;
    },
  });

  async function* values() {
    try {
      yield 1;
    } finally {
      events.push("finally");
    }
  }

  const iterator = values();
  await iterator.next();
  await expect(iterator.return(returnedValue)).resolves.toEqual({
    value: "done",
    done: true,
  });
  expect(events).toEqual(["constructor", "finally"]);
});

test("async generator queued requests wait behind awaited return fulfillment", async () => {
  const events = [];
  async function* values() {
    yield 1;
  }

  const iterator = values();
  await iterator.next();
  const returnValue = Promise.resolve().then(() => {
    events.push("return value");
    return 2;
  });
  const returned = iterator.return(returnValue).then((result) => {
    events.push("return:" + result.value + ":" + result.done);
  });
  const nexted = iterator.next().then((result) => {
    events.push("next:" + result.value + ":" + result.done);
  });

  const tick = Promise.resolve().then(() => events.push("tick"));
  await Promise.all([returned, nexted, tick]);
  expect(events).toEqual([
    "return value",
    "tick",
    "return:2:true",
    "next:undefined:true",
  ]);
});

test("async generator queued requests drain after awaited return rejection", async () => {
  const events = [];
  async function* values() {
    yield 1;
  }

  const iterator = values();
  await iterator.next();
  const returned = iterator.return(Promise.reject("boom")).then(
    () => events.push("return fulfilled"),
    (reason) => events.push("return rejected:" + reason),
  );
  const nexted = iterator.next().then((result) => {
    events.push("next:" + result.value + ":" + result.done);
  });

  await Promise.all([returned, nexted]);
  expect(events).toEqual([
    "return rejected:boom",
    "next:undefined:true",
  ]);
});

test("arrow function does NOT have own prototype property", () => {
  const f = () => 42;
  expect(f.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(f, "prototype")).toBe(false);
});

test("async arrow function does NOT have own prototype property", () => {
  const f = async () => 42;
  expect(f.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(f, "prototype")).toBe(false);
});

// ES2026 method definitions are not constructable, but generator and async
// generator methods still receive the generator function `prototype` data
// property used by yielded generator objects.

test("object method shorthand: concise method does NOT have prototype", () => {
  const obj = {
    method() {
      return 1;
    },
  };
  expect(obj.method.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(obj.method, "prototype")).toBe(false);
});

test("object method shorthand: generator method has generator prototype", () => {
  const obj = {
    *gen() {
      yield 1;
    },
  };
  expect(Object.prototype.hasOwnProperty.call(obj.gen, "prototype")).toBe(true);
  expect(Object.getPrototypeOf(obj.gen.prototype)).toBe(Object.getPrototypeOf(function* () {}).prototype);
});

test("object method shorthand: async method does NOT have prototype", () => {
  const obj = {
    async method() {
      return 1;
    },
  };
  expect(obj.method.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(obj.method, "prototype")).toBe(false);
});

test("object method shorthand: async generator method has async generator prototype", () => {
  const obj = {
    async *gen() {
      yield 1;
    },
  };
  expect(Object.prototype.hasOwnProperty.call(obj.gen, "prototype")).toBe(true);
  expect(Object.getPrototypeOf(obj.gen.prototype)).toBe(Object.getPrototypeOf(async function* () {}).prototype);
});

test("object method shorthand: getter does NOT have prototype", () => {
  const obj = {
    get value() {
      return 1;
    },
  };
  const desc = Object.getOwnPropertyDescriptor(obj, "value");
  expect(desc.get.prototype).toBeUndefined();
});

test("object method shorthand: setter does NOT have prototype", () => {
  const obj = {
    set value(v) {},
  };
  const desc = Object.getOwnPropertyDescriptor(obj, "value");
  expect(desc.set.prototype).toBeUndefined();
});

test("class instance method: concise method does NOT have prototype", () => {
  class C {
    method() {}
  }
  expect(C.prototype.method.prototype).toBeUndefined();
});

test("class instance method: generator method has generator prototype", () => {
  class C {
    *gen() {
      yield 1;
    }
  }
  expect(Object.prototype.hasOwnProperty.call(C.prototype.gen, "prototype")).toBe(true);
  expect(Object.getPrototypeOf(C.prototype.gen.prototype)).toBe(Object.getPrototypeOf(function* () {}).prototype);
});

test("class instance method: async method does NOT have prototype", () => {
  class C {
    async method() {}
  }
  expect(C.prototype.method.prototype).toBeUndefined();
});

test("class instance method: async generator method has async generator prototype", () => {
  class C {
    async *gen() {
      yield 1;
    }
  }
  expect(Object.prototype.hasOwnProperty.call(C.prototype.gen, "prototype")).toBe(true);
  expect(Object.getPrototypeOf(C.prototype.gen.prototype)).toBe(Object.getPrototypeOf(async function* () {}).prototype);
});

test("class static method: concise method does NOT have prototype", () => {
  class C {
    static method() {}
  }
  expect(C.method.prototype).toBeUndefined();
});

test("class static method: generator method has generator prototype", () => {
  class C {
    static *gen() {
      yield 1;
    }
  }
  expect(Object.prototype.hasOwnProperty.call(C.gen, "prototype")).toBe(true);
  expect(Object.getPrototypeOf(C.gen.prototype)).toBe(Object.getPrototypeOf(function* () {}).prototype);
});

test("class static method: async method does NOT have prototype", () => {
  class C {
    static async method() {}
  }
  expect(C.method.prototype).toBeUndefined();
});

test("class static method: async generator method has async generator prototype", () => {
  class C {
    static async *gen() {
      yield 1;
    }
  }
  expect(Object.prototype.hasOwnProperty.call(C.gen, "prototype")).toBe(true);
  expect(Object.getPrototypeOf(C.gen.prototype)).toBe(Object.getPrototypeOf(async function* () {}).prototype);
});
