/*---
description: Object methods preserve receiver state through this
features: [Object, method-calls, property-access]
---*/

test("object methods update and read object state through this", () => {
  const counter = {
    count: 0,
    increment() {
      this.count = this.count + 1;
      return this.count;
    },
    decrement() {
      this.count = this.count - 1;
      return this.count;
    },
    reset() {
      this.count = 0;
      return this;
    },
    getInfo() {
      return {
        current: this.count,
        isPositive: this.count > 0,
        isZero: this.count === 0,
      };
    },
  };

  expect(counter.increment()).toBe(1);
  expect(counter.increment()).toBe(2);
  expect(counter.increment()).toBe(3);
  expect(counter.getInfo()).toEqual({
    current: 3,
    isPositive: true,
    isZero: false,
  });

  expect(counter.decrement()).toBe(2);
  counter.reset();
  expect(counter.getInfo()).toEqual({
    current: 0,
    isPositive: false,
    isZero: true,
  });
});
