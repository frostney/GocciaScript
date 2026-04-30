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

test("methods added to prototype are visible to instances via [[Prototype]] chain (interpreter only — new on plain functions is not yet wired)", () => {
  // We can verify the prototype chain mechanically without invoking `new`:
  // Object.create(f.prototype) gives us an object whose [[Prototype]] is f.prototype.
  function Animal() {}
  Animal.prototype.speak = function () {
    return "generic sound";
  };

  const dog = Object.create(Animal.prototype);
  expect(dog.speak()).toBe("generic sound");
  expect(Object.getPrototypeOf(dog)).toBe(Animal.prototype);
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

test("generator function prototype is non-writable, non-configurable", () => {
  function* g() {}
  const desc = Object.getOwnPropertyDescriptor(g, "prototype");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("generator prototype assignment throws TypeError in strict mode", () => {
  function* g() {}
  expect(() => {
    g.prototype = {};
  }).toThrow(TypeError);
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

test("async generator declaration prototype descriptor matches generator (non-writable, non-configurable)", () => {
  async function* g() {}
  const desc = Object.getOwnPropertyDescriptor(g, "prototype");
  expect(typeof desc.value).toBe("object");
  expect(desc.value).not.toBeNull();
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("async generator expression prototype descriptor matches generator (non-writable, non-configurable)", () => {
  const g = async function* () {};
  const desc = Object.getOwnPropertyDescriptor(g, "prototype");
  expect(typeof desc.value).toBe("object");
  expect(desc.value).not.toBeNull();
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(false);
});

test("async generator prototype assignment throws TypeError in strict mode", () => {
  async function* g() {}
  const original = g.prototype;
  expect(() => {
    g.prototype = null;
  }).toThrow(TypeError);
  expect(g.prototype).toBe(original);
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

// ES2026 §15.4.4 MethodDefinition uses DefineMethod, which calls
// OrdinaryFunctionCreate without MakeConstructor.  None of the method-definition
// shorthand forms — concise method, generator method, async method, async
// generator method, getter, setter — receive a `prototype` property, regardless
// of whether they appear in an object literal or a class body, and regardless
// of `static`.  This block locks every variant in.

test("object method shorthand: concise method does NOT have prototype", () => {
  const obj = {
    method() {
      return 1;
    },
  };
  expect(obj.method.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(obj.method, "prototype")).toBe(false);
});

test("object method shorthand: generator method does NOT have prototype", () => {
  const obj = {
    *gen() {
      yield 1;
    },
  };
  expect(obj.gen.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(obj.gen, "prototype")).toBe(false);
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

test("object method shorthand: async generator method does NOT have prototype", () => {
  const obj = {
    async *gen() {
      yield 1;
    },
  };
  expect(obj.gen.prototype).toBeUndefined();
  expect(Object.prototype.hasOwnProperty.call(obj.gen, "prototype")).toBe(false);
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

test("class instance method: generator method does NOT have prototype", () => {
  class C {
    *gen() {
      yield 1;
    }
  }
  expect(C.prototype.gen.prototype).toBeUndefined();
});

test("class instance method: async method does NOT have prototype", () => {
  class C {
    async method() {}
  }
  expect(C.prototype.method.prototype).toBeUndefined();
});

test("class instance method: async generator method does NOT have prototype", () => {
  class C {
    async *gen() {
      yield 1;
    }
  }
  expect(C.prototype.gen.prototype).toBeUndefined();
});

test("class static method: concise method does NOT have prototype", () => {
  class C {
    static method() {}
  }
  expect(C.method.prototype).toBeUndefined();
});

test("class static method: generator method does NOT have prototype", () => {
  class C {
    static *gen() {
      yield 1;
    }
  }
  expect(C.gen.prototype).toBeUndefined();
});

test("class static method: async method does NOT have prototype", () => {
  class C {
    static async method() {}
  }
  expect(C.method.prototype).toBeUndefined();
});

test("class static method: async generator method does NOT have prototype", () => {
  class C {
    static async *gen() {
      yield 1;
    }
  }
  expect(C.gen.prototype).toBeUndefined();
});
