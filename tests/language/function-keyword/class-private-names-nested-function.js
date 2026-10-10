/*---
description: >
  A function expression nested in a class method resolves private names
  through the class body it appears in, not through the receiver's class.
features: [compat-function, classes, private-fields]
---*/

// ES2026 §10.2.3 OrdinaryFunctionCreate captures the running
// PrivateEnvironment for every function, method or not, so a nested
// function expression sees the Private Names of its own class evaluation.
const createClass = () =>
  class {
    #value;
    static #shared = "shared";
    static sharedReader() {
      return function (receiver) {
        return receiver.#shared;
      };
    }
    constructor(value) {
      this.#value = value;
    }
    static reader() {
      return function (receiver) {
        return receiver.#value;
      };
    }
    static writer() {
      return function (receiver) {
        receiver.#value = "written";
        return receiver.#value;
      };
    }
    static checker() {
      return function (receiver) {
        return #value in receiver;
      };
    }
    deepReader() {
      return function () {
        return function (receiver) {
          return receiver.#value;
        };
      };
    }
  };

test("a nested function reads, writes and checks its own class evaluation's fields", () => {
  const First = createClass();
  const first = new First("first");

  expect(First.reader()(first)).toBe("first");
  expect(First.checker()(first)).toBe(true);
  expect(First.writer()(first)).toBe("written");
  expect(First.reader()(first)).toBe("written");
  expect(first.deepReader()()(first)).toBe("written");
});

test("a nested function rejects another evaluation of the same class body", () => {
  const First = createClass();
  const Second = createClass();
  const second = new Second("second");

  expect(() => First.reader()(second)).toThrow(TypeError);
  expect(() => First.writer()(second)).toThrow(TypeError);
  expect(First.checker()(second)).toBe(false);
  expect(() => new First("first").deepReader()()(second)).toThrow(TypeError);
  expect(Second.reader()(second)).toBe("second");
});

test("a nested function reads only its own class evaluation's static private field", () => {
  const First = createClass();
  const Second = createClass();

  expect(First.sharedReader()(First)).toBe("shared");
  expect(() => First.sharedReader()(Second)).toThrow(TypeError);
});
