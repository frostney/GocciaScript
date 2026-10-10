/*---
description: Private-member errors name the member as written in the source
features: [private-fields, private-methods, private-static-fields]
---*/

const messageOf = (fn) => {
  try {
    fn();
  } catch (error) {
    return `${error.constructor.name}: ${error.message}`;
  }
  return "no error";
};

describe("private-member error messages", () => {
  class Members {
    #field = 1;
    #a$b = 2;
    #method() {
      return 1;
    }
    get #getterOnly() {
      return 1;
    }
    set #setterOnly(value) {}

    static #staticField = 1;
    static #staticMethod() {
      return 1;
    }
    static get #staticGetterOnly() {
      return 1;
    }
    static set #staticSetterOnly(value) {}

    static readField(obj) {
      return obj.#field;
    }
    static writeField(obj) {
      obj.#field = 2;
    }
    static incrementField(obj) {
      obj.#field++;
    }
    static addToField(obj) {
      obj.#field += 1;
    }
    static destructureIntoField(obj) {
      ({ value: obj.#field } = { value: 1 });
    }
    static readDollarField(obj) {
      return obj.#a$b;
    }
    static callMethod(obj) {
      return obj.#method();
    }
    static readGetterOnly(obj) {
      return obj.#getterOnly;
    }
    static readSetterOnly(obj) {
      return obj.#setterOnly;
    }
    static writeGetterOnly(obj) {
      obj.#getterOnly = 1;
    }
    static writeMethod(obj) {
      obj.#method = 1;
    }
    static readStaticField(obj) {
      return obj.#staticField;
    }
    static writeStaticField(obj) {
      obj.#staticField = 1;
    }
    static callStaticMethod(obj) {
      return obj.#staticMethod();
    }
    static readStaticGetterOnly(obj) {
      return obj.#staticGetterOnly;
    }
    static readStaticSetterOnly() {
      return Members.#staticSetterOnly;
    }
    static writeStaticGetterOnly() {
      Members.#staticGetterOnly = 1;
    }
    static writeStaticMethod() {
      Members.#staticMethod = 1;
    }
    static hasField(obj) {
      return #field in obj;
    }
  }

  const instance = new Members();

  test("reading a private field from an object without it", () => {
    expect(messageOf(() => Members.readField({}))).toBe(
      "TypeError: Private field #field is not accessible");
    expect(messageOf(() => Members.readField(1))).not.toContain("#slot:");
  });

  test("writing a private field to an object without it", () => {
    const expected = "TypeError: Private field #field is not accessible";
    expect(messageOf(() => Members.writeField({}))).toBe(expected);
    expect(messageOf(() => Members.incrementField({}))).toBe(expected);
    expect(messageOf(() => Members.addToField({}))).toBe(expected);
    expect(messageOf(() => Members.destructureIntoField({}))).toBe(expected);
  });

  test("a private name containing a dollar sign", () => {
    expect(messageOf(() => Members.readDollarField({}))).toBe(
      "TypeError: Private field #a$b is not accessible");
  });

  test("calling a private method on an object without it", () => {
    expect(messageOf(() => Members.callMethod({}))).toBe(
      "TypeError: Private field #method is not accessible");
  });

  test("private accessors without a getter or setter", () => {
    expect(messageOf(() => Members.readGetterOnly({}))).toBe(
      "TypeError: Private field #getterOnly is not accessible");
    expect(messageOf(() => Members.readSetterOnly(instance))).toBe(
      "TypeError: Private accessor #setterOnly was defined without a getter");
    expect(messageOf(() => Members.writeGetterOnly(instance))).toBe(
      "TypeError: Private accessor #getterOnly was defined without a setter");
  });

  test("assigning to a private method", () => {
    expect(messageOf(() => Members.writeMethod(instance))).toBe(
      "TypeError: Private method #method is not writable");
    expect(messageOf(() => Members.writeMethod({}))).toBe(
      "TypeError: Private field #method is not accessible");
  });

  test("static private members on an object that is not the class", () => {
    expect(messageOf(() => Members.readStaticField({}))).toBe(
      "TypeError: Private field #staticField is not accessible");
    expect(messageOf(() => Members.writeStaticField({}))).toBe(
      "TypeError: Private field #staticField is not accessible");
    expect(messageOf(() => Members.callStaticMethod({}))).toBe(
      "TypeError: Private field #staticMethod is not accessible");
    expect(messageOf(() => Members.readStaticGetterOnly({}))).toBe(
      "TypeError: Private field #staticGetterOnly is not accessible");
  });

  test("static private accessors and methods on the class", () => {
    expect(messageOf(() => Members.readStaticSetterOnly())).toBe(
      "TypeError: Private accessor #staticSetterOnly was defined without a getter");
    expect(messageOf(() => Members.writeStaticGetterOnly())).toBe(
      "TypeError: Private accessor #staticGetterOnly was defined without a setter");
    expect(messageOf(() => Members.writeStaticMethod())).toBe(
      "TypeError: Private method #staticMethod is not writable");
  });

  test("a private brand check against a primitive", () => {
    expect(Members.hasField({})).toBe(false);
    expect(messageOf(() => Members.hasField(1))).toBe(
      "TypeError: Cannot use 'in' operator to search for '#field' in 1");
  });

  test("a private member on null or undefined", () => {
    expect(messageOf(() => Members.readField(null))).not.toContain("#slot:");
    expect(messageOf(() => Members.readField(undefined))).not.toContain("#slot:");
    expect(messageOf(() => Members.writeField(null))).not.toContain("#slot:");
    expect(messageOf(() => Members.writeField(undefined))).not.toContain("#slot:");
  });
});
