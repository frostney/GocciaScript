/*---
description: >
  Legacy prototype members (Object.prototype.__proto__, __defineGetter__,
  __defineSetter__, __lookupGetter__, __lookupSetter__, and the
  toLocaleString of Number.prototype and Array.prototype) behave as ordinary
  properties from their first use, whatever that use is: a read, an
  assignment, a delete, or reflection.
features: [Object.prototype.__proto__, Object.prototype.__defineGetter__, Number.prototype.toLocaleString, Array.prototype.toLocaleString]
---*/

// Each member is used here first, before anything else in this file uses it.
const assignedLocale = () => "assigned";
Number.prototype.toLocaleString = assignedLocale;
delete Object.prototype.__lookupSetter__;
const protoDescriptor = Object.getOwnPropertyDescriptor(Object.prototype, "__proto__");
const arrayLocale = Array.prototype.toLocaleString;
const defineSetterKeyPresent = Reflect.ownKeys(Object.prototype).includes("__defineSetter__");

describe("legacy prototype members on first use", () => {
  test("an assignment before any read is kept", () => {
    expect(Number.prototype.toLocaleString).toBe(assignedLocale);
    expect((1).toLocaleString()).toBe("assigned");
  });

  test("a delete before any read removes the member", () => {
    expect("__lookupSetter__" in Object.prototype).toBe(false);
    expect(Object.getOwnPropertyDescriptor(Object.prototype, "__lookupSetter__")).toBe(undefined);
  });

  test("reflection before any read sees the accessor", () => {
    expect(typeof protoDescriptor.get).toBe("function");
    expect(typeof protoDescriptor.set).toBe("function");
    expect(protoDescriptor.enumerable).toBe(false);
    expect(protoDescriptor.configurable).toBe(true);
    expect(protoDescriptor.get.name).toBe("get __proto__");
    expect(protoDescriptor.set.name).toBe("set __proto__");
  });

  test("a direct read from the prototype is the method", () => {
    expect(typeof arrayLocale).toBe("function");
    expect(arrayLocale).toBe(Array.prototype.toLocaleString);
    expect(["a", "b"].toLocaleString()).toBe("a,b");
  });

  test("listing the keys finds a member nothing has read", () => {
    expect(defineSetterKeyPresent).toBe(true);
  });

  test("a member is the same function on every read", () => {
    expect(Object.prototype.__lookupGetter__).toBe(Object.prototype.__lookupGetter__);
    expect(Object.prototype.__defineGetter__).toBe(Object.prototype.__defineGetter__);
  });

  test("the members keep their names, lengths and attributes", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Object.prototype, "__defineGetter__");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
    expect(descriptor.value.name).toBe("__defineGetter__");
    expect(descriptor.value.length).toBe(2);
    expect(Object.prototype.__lookupGetter__.length).toBe(1);
  });

  test("the members show native code, not their source", () => {
    expect(String(Object.prototype.__defineGetter__)).toBe("function __defineGetter__() { [native code] }");
    expect(String(protoDescriptor.get)).toBe("function get __proto__() { [native code] }");
    expect(String(Array.prototype.toLocaleString)).toBe("function toLocaleString() { [native code] }");
  });

  test("an object literal with a member's name is an ordinary own property", () => {
    const literal = { __defineGetter__: 1, __lookupGetter__() { return 2; } };
    expect(literal.__defineGetter__).toBe(1);
    expect(literal.__lookupGetter__()).toBe(2);
    expect(typeof Object.prototype.__defineGetter__).toBe("function");
  });
});
