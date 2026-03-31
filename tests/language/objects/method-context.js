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

test("extracted methods use the new receiver when called with call()", () => {
  const counter = {
    count: 1,
    increment() {
      this.count = this.count + 1;
      return this.count;
    },
  };

  const increment = counter.increment;
  const other = { count: 10 };

  expect(increment.call(other)).toBe(11);
  expect(other.count).toBe(11);
  expect(counter.count).toBe(1);
});

test("arrow callbacks inside methods keep the method receiver", () => {
  const obj = {
    factor: 4,
    values: [1, 2, 3],
    scale() {
      return this.values.map((value) => value * this.factor);
    },
  };

  expect(obj.scale()).toEqual([4, 8, 12]);
});
