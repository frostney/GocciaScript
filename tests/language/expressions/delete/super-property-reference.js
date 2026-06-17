test("delete super property throws ReferenceError", () => {
  class Base {
    method() {
      return 1;
    }
  }

  class Derived extends Base {
    method() {
      delete super.method;
    }
  }

  expect(() => new Derived().method()).toThrow(ReferenceError);
});

test("delete super computed property does not coerce key", () => {
  const key = {
    toString() {
      throw new Error("key coerced");
    }
  };

  const object = {
    method() {
      delete super[key];
    }
  };

  expect(() => object.method()).toThrow(ReferenceError);
});
