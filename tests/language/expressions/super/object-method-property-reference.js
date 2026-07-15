test("object method super reads from home object's prototype", () => {
  const proto = { value: "ok" };
  const object = {
    __proto__: proto,
    method() {
      return super.value;
    }
  };

  expect(object.method()).toBe("ok");
});

test("super computed property resolves base before ToPropertyKey", () => {
  const proto = { p: "ok" };
  const proto2 = { p: "bad" };
  let object;
  const key = {
    toString() {
      Object.setPrototypeOf(object, proto2);
      return "p";
    }
  };
  object = {
    __proto__: proto,
    method() {
      return super[key];
    }
  };

  expect(object.method()).toBe("ok");
});

test("super computed property accepts a symbol produced by ToPropertyKey", () => {
  const symbol = Symbol("value");
  const proto = { [symbol]: "ok" };
  const key = {
    [Symbol.toPrimitive]() {
      return symbol;
    },
  };
  const object = {
    __proto__: proto,
    method() {
      return super[key];
    },
  };

  expect(object.method()).toBe("ok");
});

test("super computed assignment evaluates rhs before ToPropertyKey", () => {
  const order = [];
  const proto = {};
  const key = {
    toString() {
      order.push("key");
      return "value";
    }
  };
  const rhs = () => {
    order.push("rhs");
    return 42;
  };
  const object = {
    __proto__: proto,
    method() {
      super[key] = rhs();
    }
  };

  object.method();

  expect(order).toEqual(["rhs", "key"]);
  expect(object.value).toBe(42);
});

test("non-strict object method super assignment does not throw on failed set", () => {
  const object = {
    method() {
      super.x = 1;
      Object.freeze(object);
      super.y = 2;
    }
  };

  object.method();

  expect(Object.prototype.hasOwnProperty.call(object, "x")).toBe(true);
  expect(Object.prototype.hasOwnProperty.call(object, "y")).toBe(false);
});
