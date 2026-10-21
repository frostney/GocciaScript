/*---
description: >
  A store whose first touch of a lazily installed legacy member loads that
  member keeps the value it stores. Loading a member runs bytecode, and a
  collection there must not free a value that only the store holds: a number
  boxed out of a register, or a descriptor value a getter produced.
features: [Object.prototype.__proto__, Object.prototype.__defineGetter__, Number.prototype.toLocaleString, Reflect]
---*/

// Each member is touched here first, before anything else in this file uses
// it. The values are computed so that none of them is a constant the compiler
// could keep alive.
const seed = [1.25, 2.25, 3.25, 4.25];
const inherited = {};
inherited.__defineGetter__ = seed[0] + 0.25;

class Plain {}
const instance = new Plain();
instance.__lookupGetter__ = seed[1] + 0.25;

Number.prototype.toLocaleString = seed[2] + 0.25;

const target = {};
const receiver = {};
const reflectStored = Reflect.set(target, "__defineSetter__", seed[3] + 0.25, receiver);

const protoTarget = {};
protoTarget.__proto__ = seed[0] + 1;
const protoAfterNumber = Object.getPrototypeOf(protoTarget);
protoTarget.__proto__ = { marker: `m${seed[0]}` };

Object.defineProperty(Object.prototype, "__lookupSetter__", {
  get value() {
    return { marker: `d${seed[1]}` };
  },
  writable: true,
  configurable: true,
});

globalThis.parseFloat = seed[2] * 2;

describe("legacy members first touched by a store", () => {
  test("an assignment that finds an inherited data member defines an own property", () => {
    expect(Object.hasOwn(inherited, "__defineGetter__")).toBe(true);
    expect(inherited.__defineGetter__).toBe(1.5);
    expect(typeof Object.prototype.__defineGetter__).toBe("function");
  });

  test("an assignment on a class instance keeps its value", () => {
    expect(instance.__lookupGetter__).toBe(2.5);
    expect(typeof Object.prototype.__lookupGetter__).toBe("function");
  });

  test("an assignment that replaces the member itself keeps its value", () => {
    expect(Number.prototype.toLocaleString).toBe(3.5);
  });

  test("Reflect.set with a receiver keeps its value", () => {
    expect(reflectStored).toBe(true);
    expect(receiver.__defineSetter__).toBe(4.5);
    expect(Object.hasOwn(target, "__defineSetter__")).toBe(false);
  });

  test("an assignment through the __proto__ setter ignores a number and sets an object", () => {
    expect(protoAfterNumber).toBe(Object.prototype);
    expect(Object.getPrototypeOf(protoTarget).marker).toBe("m1.25");
  });

  test("a define over the member keeps the value its descriptor produced", () => {
    expect(Object.prototype.__lookupSetter__.marker).toBe("d2.25");
    delete Object.prototype.__lookupSetter__;
  });

  test("an assignment to a lazily installed global keeps its value", () => {
    expect(parseFloat).toBe(6.5);
  });
});
