/*---
description: Field initializers run in source order
features: [classes, private-fields]
---*/

test("field initializers run in source order", () => {
  const order = [];

  class Foo {
    a = order.push("a");
    #b = order.push("b");
    c = order.push("c");
  }

  new Foo();
  expect(order.length).toBe(3);
  expect(order[0]).toBe("a");
  expect(order[1]).toBe("b");
  expect(order[2]).toBe("c");
});

test("many public, private, and inherited field initializers run in declaration order", () => {
  const order = [];

  class Base {
    a = order.push("a");
    #b = order.push("b");
    c = order.push("c");
    baseB() {
      return this.#b;
    }
  }

  class Derived extends Base {
    d = order.push("d");
    #e = order.push("e");
    f = order.push("f");
    g = order.push("g");
    derivedE() {
      return this.#e;
    }
  }

  const obj = new Derived();

  expect(order).toEqual(["a", "b", "c", "d", "e", "f", "g"]);
  expect(obj.a).toBe(1);
  expect(obj.baseB()).toBe(2);
  expect(obj.c).toBe(3);
  expect(obj.d).toBe(4);
  expect(obj.derivedE()).toBe(5);
  expect(obj.f).toBe(6);
  expect(obj.g).toBe(7);
});
