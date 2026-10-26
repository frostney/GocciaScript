/*---
description: Reading the length of an Array instance
features: [Array]
---*/

const lengthOf = (array) => array.length;

test("length follows elements added and removed", () => {
  const array = [];
  const seen = [];
  for (const value of [1, 2, 3]) {
    array.push(value);
    seen.push(array.length);
  }
  array.pop();
  seen.push(lengthOf(array));
  array.length = 0;
  seen.push(lengthOf(array));
  expect(seen).toEqual([1, 2, 3, 2, 0]);
});

test("length counts holes and a length set past the elements", () => {
  const holey = [1, , 3];
  expect(lengthOf(holey)).toBe(3);
  const grown = [1];
  grown.length = 5;
  expect(lengthOf(grown)).toBe(5);
  expect(grown[4]).toBe(undefined);
  const far = [];
  far[9] = "x";
  expect(lengthOf(far)).toBe(10);
  expect(lengthOf(new Array(7))).toBe(7);
});

test("length read through an object property matches the array", () => {
  const holder = { items: [4, 5, 6] };
  const total = () => {
    let sum = 0;
    for (const index of [0, 1, 2, 3]) {
      if (index < holder.items.length) {
        sum += holder.items[index];
      }
    }
    return sum;
  };
  expect(total()).toBe(15);
  holder.items.push(7);
  expect(total()).toBe(22);
});

test("a frozen array and a non-writable length still report the length", () => {
  const frozen = Object.freeze([1, 2]);
  expect(lengthOf(frozen)).toBe(2);
  const fixed = [1, 2, 3];
  Object.defineProperty(fixed, "length", { writable: false });
  expect(lengthOf(fixed)).toBe(3);
  expect(() => fixed.push(4)).toThrow(TypeError);
  expect(lengthOf(fixed)).toBe(3);
});

test("a subclass instance and a proxy report the length", () => {
  class Stack extends Array {}
  const stack = new Stack();
  stack.push(1, 2);
  expect(lengthOf(stack)).toBe(2);

  let reads = 0;
  const proxy = new Proxy([1, 2, 3], {
    get(target, key, receiver) {
      if (key === "length") {
        reads++;
        return 42;
      }
      return Reflect.get(target, key, receiver);
    },
  });
  expect(lengthOf(proxy)).toBe(42);
  expect(reads).toBe(1);
});

test("a length on the prototype chain does not shadow an array's own length", () => {
  const array = [1, 2, 3];
  Object.setPrototypeOf(array, {
    get length() {
      return 99;
    },
  });
  expect(lengthOf(array)).toBe(3);
});
