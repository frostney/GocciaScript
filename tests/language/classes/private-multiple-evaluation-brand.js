/*---
description: Re-evaluated classes create distinct private brands
features: [classes, private-fields, private-methods, private-static-fields]
---*/

test("private instance methods are branded per class evaluation", () => {
  const createInstance = () => {
    const C = class {
      #method() {
        return "ok";
      }

      access(receiver) {
        return receiver.#method();
      }
    };
    return new C();
  };

  const first = createInstance();
  const second = createInstance();

  expect(first.access(first)).toBe("ok");
  expect(second.access(second)).toBe("ok");
  expect(() => first.access(second)).toThrow(TypeError);
  expect(() => second.access(first)).toThrow(TypeError);
});

test("private instance accessors are branded per class evaluation", () => {
  const createInstance = () => {
    const C = class {
      #value = "ok";

      get #reader() {
        return this.#value;
      }

      set #writer(value) {
        this.#value = value;
      }

      read(receiver) {
        return receiver.#reader;
      }

      write(receiver, value) {
        receiver.#writer = value;
      }
    };
    return new C();
  };

  const first = createInstance();
  const second = createInstance();

  expect(first.read(first)).toBe("ok");
  first.write(first, "changed");
  expect(first.read(first)).toBe("changed");
  expect(() => first.read(second)).toThrow(TypeError);
  expect(() => first.write(second, "bad")).toThrow(TypeError);
});

test("private static fields are branded per class evaluation", () => {
  const createClass = () => class {
    static #value = "ok";

    static access() {
      return this.#value;
    }
  };

  const First = createClass();
  const Second = createClass();

  expect(First.access()).toBe("ok");
  expect(Second.access()).toBe("ok");
  expect(() => First.access.call(Second)).toThrow(TypeError);
  expect(() => Second.access.call(First)).toThrow(TypeError);
});

test("private static methods and accessors are branded per class evaluation", () => {
  const createClass = () => class {
    static #value = "ok";

    static #method() {
      return this.#value;
    }

    static get #reader() {
      return this.#value;
    }

    static methodAccess() {
      return this.#method();
    }

    static getterAccess() {
      return this.#reader;
    }
  };

  const First = createClass();
  const Second = createClass();

  expect(First.methodAccess()).toBe("ok");
  expect(Second.getterAccess()).toBe("ok");
  expect(() => First.methodAccess.call(Second)).toThrow(TypeError);
  expect(() => Second.getterAccess.call(First)).toThrow(TypeError);
});
