/*---
description: Prototype properties set to undefined shadow class methods
features: [class, Object.defineProperty]
---*/

test("class prototype property set to undefined shadows a method on cold read", () => {
  class Base {
    value() {
      return "method";
    }
  }

  Base.prototype.value = undefined;

  expect(new Base().value).toBeUndefined();
});

test("inherited class prototype property set to undefined shadows superclass method", () => {
  class Base {
    value() {
      return "base";
    }
  }
  class Derived extends Base {}

  Object.defineProperty(Base.prototype, "value", {
    value: undefined,
    configurable: true,
    writable: true,
  });

  expect(new Derived().value).toBeUndefined();
});

test("warmed class method read returns undefined after prototype shadow", () => {
  class Counter {
    value() {
      return 1;
    }
  }
  const counters = Array.from({ length: 64 }, () => new Counter());
  const readValue = (counter) => counter.value;

  counters.forEach((counter) => {
    expect(typeof readValue(counter)).toBe("function");
  });

  Counter.prototype.value = undefined;

  counters.forEach((counter) => {
    expect(readValue(counter)).toBeUndefined();
  });
});
