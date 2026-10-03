/*---
description: >
  Object literal methods inside a class body resolve private names through the
  enclosing class
features: [classes, private-fields, private-methods]
---*/

// ES2026 §9.2.1.2 ResolvePrivateIdentifier walks the PrivateEnvironment the
// method was created in; §7.3.30 PrivateGet throws a TypeError when the
// receiver lacks the Private Name, and §13.10.1 `#x in o` returns a boolean.
class Other {
  #value = "other";
}

class Owner {
  #value = "owner";
  static #peek(receiver) {
    return receiver.#value;
  }
  static methods() {
    return {
      read(receiver) {
        return receiver.#value;
      },
      write(receiver, value) {
        receiver.#value = value;
        return receiver.#value;
      },
      has(receiver) {
        return #value in receiver;
      },
      peek(receiver) {
        return Owner.#peek(receiver);
      },
      nested: {
        read(receiver) {
          return receiver.#value;
        },
      },
    };
  }
  instanceMethods() {
    return {
      has(receiver) {
        return #value in receiver;
      },
    };
  }
}

test("reads and writes the enclosing class's field", () => {
  const methods = Owner.methods();
  const owner = new Owner();

  expect(methods.read(owner)).toBe("owner");
  expect(methods.write(owner, "changed")).toBe("changed");
  expect(methods.nested.read(owner)).toBe("changed");
  expect(methods.peek(owner)).toBe("changed");
});

test("does not reach a same-named field of another class", () => {
  const methods = Owner.methods();
  const other = new Other();

  expect(() => methods.read(other)).toThrow(TypeError);
  expect(() => methods.write(other, "w")).toThrow(TypeError);
  expect(() => methods.nested.read(other)).toThrow(TypeError);
  expect(() => methods.peek(other)).toThrow(TypeError);
});

test("#x in returns a boolean for every object operand", () => {
  const methods = Owner.methods();
  const instanceMethods = new Owner().instanceMethods();

  expect(methods.has(new Owner())).toBe(true);
  expect(methods.has(new Other())).toBe(false);
  expect(methods.has({})).toBe(false);
  expect(instanceMethods.has(new Owner())).toBe(true);
  expect(instanceMethods.has({})).toBe(false);
});
