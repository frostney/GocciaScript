/*---
description: Assigning to a private method or a getter-only private accessor throws and leaves it unchanged
features: [private-fields, private-methods, private-static-fields, class-inheritance]
---*/

const messageOf = (fn) => {
  try {
    fn();
  } catch (error) {
    return `${error.constructor.name}: ${error.message}`;
  }
  return "no error";
};

describe("assigning to a private method", () => {
  class ReturnOverrideBase {
    constructor(obj) {
      return obj;
    }
  }

  class Members extends ReturnOverrideBase {
    #field = 1;
    #method() {
      return "method";
    }
    get #getterOnly() {
      return "getter";
    }

    static writeMethod(obj) {
      obj.#method = 1;
    }
    static addToMethod(obj) {
      obj.#method += 1;
    }
    static callMethod(obj) {
      return obj.#method();
    }
    static writeGetterOnly(obj) {
      obj.#getterOnly = 1;
    }
    static readGetterOnly(obj) {
      return obj.#getterOnly;
    }
    static writeField(obj, value) {
      obj.#field = value;
      return obj.#field;
    }
  }

  test("an instance keeps its private method", () => {
    class Own {
      #method() {
        return "own method";
      }
      static writeMethod(obj) {
        obj.#method = 1;
      }
      static callMethod(obj) {
        return obj.#method();
      }
    }
    const instance = new Own();
    expect(messageOf(() => Own.writeMethod(instance))).toBe(
      "TypeError: Private method #method is not writable");
    expect(Own.callMethod(instance)).toBe("own method");
  });

  test("an object returned from a base constructor keeps its private method", () => {
    const obj = {};
    new Members(obj);
    expect(messageOf(() => Members.writeMethod(obj))).toBe(
      "TypeError: Private method #method is not writable");
    expect(messageOf(() => Members.addToMethod(obj))).toBe(
      "TypeError: Private method #method is not writable");
    expect(Members.callMethod(obj)).toBe("method");
  });

  test("an object returned from a base constructor keeps its getter-only accessor", () => {
    const obj = {};
    new Members(obj);
    expect(messageOf(() => Members.writeGetterOnly(obj))).toBe(
      "TypeError: Private accessor #getterOnly was defined without a setter");
    expect(Members.readGetterOnly(obj)).toBe("getter");
  });

  test("an object returned from a base constructor still accepts private field writes", () => {
    const obj = {};
    new Members(obj);
    expect(Members.writeField(obj, 2)).toBe(2);
    Object.freeze(obj);
    expect(Members.writeField(obj, 3)).toBe(3);
    expect(messageOf(() => Members.writeMethod(obj))).toBe(
      "TypeError: Private method #method is not writable");
    expect(Members.callMethod(obj)).toBe("method");
  });

  test("an object without the brand is rejected as inaccessible", () => {
    expect(messageOf(() => Members.writeMethod({}))).toBe(
      "TypeError: Private field #method is not accessible");
  });

  test("a class keeps its static private method", () => {
    class Statics {
      static #method() {
        return "static method";
      }
      static writeMethod(target) {
        target.#method = 1;
      }
      static callMethod(target) {
        return target.#method();
      }
    }
    class Derived extends Statics {}

    expect(messageOf(() => Statics.writeMethod(Statics))).toBe(
      "TypeError: Private method #method is not writable");
    expect(Statics.callMethod(Statics)).toBe("static method");
    expect(messageOf(() => Statics.writeMethod(Derived))).toBe(
      "TypeError: Private field #method is not accessible");
    expect(Statics.callMethod(Statics)).toBe("static method");
  });
});
