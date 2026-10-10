/*---
description: Instance auto-accessors are initialized together with fields, in source order
features: [decorators, auto-accessor, class-fields-public, class-fields-private]
---*/

// proposal-decorators InitializeInstanceElements runs InitializeFieldOrAccessor
// for every element of kind ~field~ or ~accessor~, in source order.

describe("auto-accessor initialization order", () => {
  test("an accessor after a field is initialized", () => {
    class C {
      y = 2;
      accessor x = 1;
    }

    expect(new C().x).toBe(1);
  });

  test("an accessor before a field is initialized", () => {
    class C {
      accessor x = 1;
      y = 2;
    }

    expect(new C().x).toBe(1);
  });

  test("a private accessor next to a field is initialized", () => {
    class C {
      y = 2;
      accessor #x = 1;
      read() {
        return this.#x;
      }
    }

    expect(new C().read()).toBe(1);
  });

  test("a field after an accessor reads its value", () => {
    class C {
      accessor x = 1;
      b = this.x + 1;
    }

    expect(new C().b).toBe(2);
  });

  test("a field after a private accessor reads its value", () => {
    class C {
      accessor #x = 1;
      b = this.#x + 1;
    }

    expect(new C().b).toBe(2);
  });

  test("an accessor after an accessor reads its value", () => {
    class C {
      accessor x = 1;
      accessor w = this.x + 1;
    }

    expect(new C().w).toBe(2);
  });

  test("initializers run once each, in declaration order", () => {
    const order = [];
    class C {
      a = order.push("a");
      accessor b = order.push("b");
      #c = order.push("#c");
      accessor #d = order.push("#d");
      [`e`] = order.push("e");
      accessor [`f`] = order.push("f");
    }

    new C();

    expect(order).toEqual(["a", "b", "#c", "#d", "e", "f"]);
  });

  test("a subclass initializes its accessors after the superclass constructor returns", () => {
    const order = [];
    class Base {
      accessor base = order.push("base");
      constructor() {
        order.push("base constructor");
      }
    }
    class Derived extends Base {
      accessor derived = order.push("derived");
      constructor() {
        super();
        order.push("derived constructor");
      }
    }

    new Derived();

    expect(order).toEqual(["base", "base constructor", "derived", "derived constructor"]);
  });
});
