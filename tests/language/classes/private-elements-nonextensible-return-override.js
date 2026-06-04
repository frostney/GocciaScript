/*---
description: Private elements cannot be added to non-extensible constructor return overrides
features: [private-fields, private-methods, class-inheritance]
---*/

describe("Private elements on constructor return overrides", () => {
  class ReturnOverrideBase {
    constructor(obj) {
      return obj;
    }
  }

  test("private fields throw when the replacement object is non-extensible", () => {
    class WithPrivateField extends ReturnOverrideBase {
      #value;

      constructor(obj) {
        super(obj);
        this.#value = 42;
      }

      static read(obj) {
        return obj.#value;
      }
    }

    expect(() => new WithPrivateField(Object.preventExtensions({}))).toThrow(TypeError);

    const obj = {};
    new WithPrivateField(obj);
    expect(WithPrivateField.read(obj)).toBe(42);
  });

  test("private field fallback does not brand unrelated constructor arguments", () => {
    class WithPrivateField extends ReturnOverrideBase {
      #value;

      constructor(obj, other) {
        super(obj);
        other.#value = 42;
      }
    }

    expect(() => new WithPrivateField({}, {})).toThrow(TypeError);
  });

  test("private methods throw when the replacement object is non-extensible", () => {
    class WithPrivateMethod extends ReturnOverrideBase {
      #method() {
        return 42;
      };

      constructor(obj) {
        super(obj);
      }

      static call(obj) {
        return obj.#method();
      }
    }

    expect(() => new WithPrivateMethod(Object.preventExtensions({}))).toThrow(TypeError);

    const obj = {};
    new WithPrivateMethod(obj);
    expect(WithPrivateMethod.call(obj)).toBe(42);
  });

  test("private accessors throw when the replacement object is non-extensible", () => {
    class WithPrivateAccessor extends ReturnOverrideBase {
      get #accessor() {
        return 7;
      };

      constructor(obj) {
        super(obj);
      }

      static read(obj) {
        return obj.#accessor;
      }
    }

    expect(() => new WithPrivateAccessor(Object.preventExtensions({}))).toThrow(TypeError);

    const obj = {};
    new WithPrivateAccessor(obj);
    expect(WithPrivateAccessor.read(obj)).toBe(7);
  });
});
