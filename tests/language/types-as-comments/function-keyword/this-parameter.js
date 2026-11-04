/*---
description: A `this` parameter types `this` and is not a parameter at run time
features: [types-as-comments]
---*/

function total(this: { base: number }, extra: number): number {
  return this.base + extra;
}

function onlyThis(this: { base: number }) {
  return this.base;
}

test("a function's this parameter is not counted or bound", () => {
  expect(total.call({ base: 40 }, 2)).toBe(42);
  expect(total.length).toBe(1);
  expect(onlyThis.call({ base: 5 })).toBe(5);
  expect(onlyThis.length).toBe(0);
});

test("methods and function expressions take a this parameter", () => {
  const counter = {
    count: 2,
    next(this: { count: number }) {
      return this.count + 1;
    },
  };
  class Box {
    value = 9;
    read(this: Box, offset: number) {
      return this.value + offset;
    }
  }
  const named = function (this: { name: string }, suffix: string) {
    return this.name + suffix;
  };

  expect(counter.next()).toBe(3);
  expect(new Box().read(1)).toBe(10);
  expect(named.call({ name: "a" }, "b")).toBe("ab");
});
