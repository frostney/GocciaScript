/*---
description: Non-strict compatibility assignment semantics
features: [compat-non-strict-mode]
---*/

describe("non-strict assignment", () => {
  test("read-only property writes silently fail and return the assigned value", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (obj.fixed = 2);

    expect(result).toBe(2);
    expect(obj.fixed).toBe(1);
  });

  test("computed read-only property writes silently fail", () => {
    const obj = {};
    const key = "fixed";
    Object.defineProperty(obj, key, {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (obj[key] = 2);

    expect(result).toBe(2);
    expect(obj.fixed).toBe(1);
  });

  test("compound read-only property writes return the computed value", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (obj.fixed += 3);

    expect(result).toBe(4);
    expect(obj.fixed).toBe(1);
  });

  test("update read-only property writes return update expression values", () => {
    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const postfix = obj.fixed++;
    const prefix = ++obj.fixed;

    expect(postfix).toBe(1);
    expect(prefix).toBe(2);
    expect(obj.fixed).toBe(1);
  });

  test("with object writes use non-strict object-environment assignment", () => {
    const obj = {};
    let result = 0;
    Object.defineProperty(obj, "value", {
      value: 1,
      writable: false,
      configurable: true,
    });

    with (obj) {
      result = (value = 2);
    }

    expect(result).toBe(2);
    expect(obj.value).toBe(1);
  });

  test("global object identifier writes silently fail for non-writable properties", () => {
    Object.defineProperty(globalThis, "__gocciaNonStrictFixedAssignment", {
      value: 1,
      writable: false,
      configurable: true,
    });

    const result = (__gocciaNonStrictFixedAssignment = 2);

    expect(result).toBe(2);
    expect(globalThis.__gocciaNonStrictFixedAssignment).toBe(1);

    delete globalThis.__gocciaNonStrictFixedAssignment;
  });

  test("unresolvable identifier assignment creates a global object property", () => {
    delete globalThis.__gocciaLooseCreatedAssignment;

    try {
      const result = (__gocciaLooseCreatedAssignment = 3);

      expect(result).toBe(3);
      expect(globalThis.__gocciaLooseCreatedAssignment).toBe(3);
      expect(__gocciaLooseCreatedAssignment).toBe(3);
    } finally {
      delete globalThis.__gocciaLooseCreatedAssignment;
    }
  });

  test("non-extensible object additions silently fail", () => {
    const obj = {};
    Object.preventExtensions(obj);

    const result = (obj.added = 1);

    expect(result).toBe(1);
    expect(Object.hasOwn(obj, "added")).toBe(false);
  });

  test("computed array index writes create elements visible to array methods", () => {
    const array = [];

    array[0] = 0;
    array[3] = 3;

    expect(0 in array).toBe(true);
    expect(3 in array).toBe(true);
    expect(array.length).toBe(4);
    expect(array[3]).toBe(3);

    expect(array.pop()).toBe(3);
    expect(array.length).toBe(3);
    expect(3 in array).toBe(false);
    expect(array[3]).toBeUndefined();
    expect(array[2]).toBeUndefined();
  });

  test("array index deletion side effects are visible during indexOf", () => {
    const array = [];
    array[10] = "ten";
    array.length = 20;

    const fromIndex = {
      valueOf() {
        delete array[10];
        return 3;
      },
    };

    expect(array.indexOf("ten", fromIndex)).toBe(-1);
    expect(10 in array).toBe(false);
    expect(array[10]).toBeUndefined();
  });

  test("class constructor static setters receive non-strict assignments", () => {
    let captured = 0;
    const computed = "computed";
    const symbolKey = Symbol("staticSetter");

    class C {
      static set eval(value) {
        captured = captured + value;
      }

      static set arguments(value) {
        captured = captured + value * 10;
      }

      static set [computed](value) {
        captured = captured + value * 100;
      }

      static set [symbolKey](value) {
        captured = captured + value * 1000;
      }
    }

    const evalDescriptor = Object.getOwnPropertyDescriptor(C, "eval");
    const symbolDescriptor = Object.getOwnPropertyDescriptor(C, symbolKey);

    C.eval = 1;
    C.arguments = 2;
    C[computed] = 3;
    C[symbolKey] = 4;

    expect(captured).toBe(4321);
    expect(typeof evalDescriptor.set).toBe("function");
    expect(evalDescriptor.enumerable).toBe(false);
    expect(evalDescriptor.configurable).toBe(true);
    expect(typeof symbolDescriptor.set).toBe("function");
    expect(symbolDescriptor.enumerable).toBe(false);
    expect(symbolDescriptor.configurable).toBe(true);
  });

  test("inherited static setters receive the derived constructor as receiver", () => {
    class Base {
      static set value(next) {
        this.received = next;
      }
    }

    class Derived extends Base {}

    Derived.value = 7;

    expect(Derived.received).toBe(7);
    expect(Base.received).toBeUndefined();
  });
});

// ES2026 §10.1.9.2 OrdinarySetWithOwnDescriptor steps 3-7 with §6.2.5.6
// PutValue step 3.e: assigning to an own accessor that has no setter is a
// silent no-op in non-strict code. The accessor is never replaced.
describe("non-strict assignment to an own accessor", () => {
  class Plain {}

  const receivers = [
    ["a plain object", () => ({})],
    ["a class instance", () => new Plain()],
    ["an array", () => [1, 2, 3]],
    ["a Uint8Array", () => new Uint8Array(2)],
    ["a Map", () => new Map()],
    ["a String object", () => new String("ab")],
  ];

  describe.each(receivers)("%s", (label, make) => {
    test("a getter-only accessor silently keeps its value", () => {
      const receiver = make();
      Object.defineProperty(receiver, "x", {
        get: () => "from getter",
        configurable: true,
      });

      const result = (receiver.x = 1);

      expect(result).toBe(1);
      expect(receiver.x).toBe("from getter");
      expect("get" in Object.getOwnPropertyDescriptor(receiver, "x")).toBe(true);
    });

    test("a setter is called with the receiver as this", () => {
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
      expect("set" in Object.getOwnPropertyDescriptor(receiver, "x")).toBe(true);
    });

    test("Object.assign still throws TypeError for a getter-only accessor", () => {
      const receiver = make();
      Object.defineProperty(receiver, "x", {
        get: () => "from getter",
        configurable: true,
      });

      expect(() => Object.assign(receiver, { x: 1 })).toThrow(TypeError);
      expect(receiver.x).toBe("from getter");
    });
  });

  test("a compound assignment to a getter-only accessor of a class instance is a no-op", () => {
    const receiver = new Plain();
    Object.defineProperty(receiver, "x", { get: () => 1, configurable: true });

    const result = (receiver.x += 5);

    expect(result).toBe(6);
    expect(receiver.x).toBe(1);
    expect("get" in Object.getOwnPropertyDescriptor(receiver, "x")).toBe(true);
  });

  test("a write inside a class body throws TypeError, because class code is strict", () => {
    class ReadOnly {
      constructor() {
        Object.defineProperty(this, "x", { get: () => 1, configurable: true });
      }

      write() {
        this.x = 2;
      }
    }
    const receiver = new ReadOnly();

    expect(() => receiver.write()).toThrow(TypeError);
    expect(receiver.x).toBe(1);
  });
});
