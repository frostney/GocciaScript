/*---
description: Under strict types, a Script's top-level var keeps its type on the global object's property, so every write to it is checked
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

var count: number = 1;
var label = "start";
var untyped = label;
var pending: string;

let lateError;
try {
  var late: number = "text";
} catch (error) {
  lateError = error;
}

describe("top-level var under strict types", () => {
  test("a rejected initializer throws and leaves the var undefined", () => {
    expect(lateError).toBeInstanceOf(TypeError);
    expect(late).toBe(undefined);
  });

  test("an assignment of the annotated type is stored", () => {
    count = 2;
    expect(count).toBe(2);
    expect(globalThis.count).toBe(2);
  });

  test("an assignment of another type throws and keeps the value", () => {
    count = 3;
    expect(() => { count = "text"; }).toThrow(TypeError);
    expect(count).toBe(3);
  });

  test("a closure's assignment is checked", () => {
    const set = (value) => { count = value; };
    set(4);
    expect(count).toBe(4);
    expect(() => set(true)).toThrow(TypeError);
    expect(count).toBe(4);
  });

  test("a property write on globalThis is checked", () => {
    expect(() => { globalThis.count = "text"; }).toThrow(TypeError);
    expect(() => { globalThis["count"] = "text"; }).toThrow(TypeError);
    globalThis.count = 5;
    expect(count).toBe(5);
  });

  test("reflective writes are checked", () => {
    expect(() => Reflect.set(globalThis, "count", "text")).toThrow(TypeError);
    expect(() => Object.defineProperty(globalThis, "count", { value: "text" })).toThrow(TypeError);
    expect(() => Object.assign(globalThis, { count: "text" })).toThrow(TypeError);
    expect(count).toBe(5);
    expect(Reflect.set(globalThis, "count", 6)).toBe(true);
    expect(count).toBe(6);
  });

  test("an update expression is checked", () => {
    count = 7;
    count++;
    expect(count).toBe(8);
    expect(() => { count += "1"; }).toThrow(TypeError);
    expect(count).toBe(8);
  });

  test("a repeated store from a hot closure is still checked", () => {
    const set = (value) => { count = value; };
    for (const step of Array.from({ length: 5000 }, (_, index) => index)) set(step);
    expect(count).toBe(4999);
    expect(() => set("text")).toThrow(TypeError);
    expect(count).toBe(4999);
  });

  test("an unannotated var takes the type its literal initializer implies", () => {
    expect(() => { label = 1; }).toThrow(TypeError);
    label = "next";
    expect(label).toBe("next");
  });

  test("a var whose initializer implies no type stays untyped", () => {
    untyped = 1;
    untyped = true;
    expect(untyped).toBe(true);
  });

  test("a declaration without an initializer holds undefined and checks later stores", () => {
    expect(pending).toBe(undefined);
    expect(() => { pending = 1; }).toThrow(TypeError);
    pending = "set";
    expect(pending).toBe("set");
  });

  test("the property stays a data property of the global object", () => {
    const descriptor = Object.getOwnPropertyDescriptor(globalThis, "count");
    expect(descriptor.writable).toBe(true);
    expect(descriptor.enumerable).toBe(true);
    expect(descriptor.configurable).toBe(false);
  });
});
