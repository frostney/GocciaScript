/*---
description: Array.prototype.values returns an iterator over array values
features: [Array.prototype.values, Iterator]
---*/

test("returns an iterator over array values", () => {
  const arr = [10, 20, 30];
  const iter = arr.values();
  expect(iter.next().value).toBe(10);
  expect(iter.next().value).toBe(20);
  expect(iter.next().value).toBe(30);
  expect(iter.next().done).toBe(true);
});

test("done is false until exhausted", () => {
  const iter = [1].values();
  const first = iter.next();
  expect(first.value).toBe(1);
  expect(first.done).toBe(false);
  expect(iter.next().done).toBe(true);
});

test("returns undefined value when done", () => {
  const iter = [].values();
  const result = iter.next();
  expect(result.value).toBe(undefined);
  expect(result.done).toBe(true);
});

test("works with spread operator", () => {
  const arr = [1, 2, 3];
  expect([...arr.values()]).toEqual([1, 2, 3]);
});

test("generic receiver iterates array-like values", () => {
  const obj = { 0: 'a', 1: 'b', length: 2 };
  const iter = Array.prototype.values.call(obj);
  expect(iter.next().value).toBe('a');
  expect(iter.next().value).toBe('b');
  expect(iter.next().done).toBe(true);
});

test("array iterator prototype owns next with spec descriptor", () => {
  const proto = Object.getPrototypeOf([].values());
  const descriptor = Object.getOwnPropertyDescriptor(proto, "next");

  expect(Object.prototype.hasOwnProperty.call(proto, "next")).toBe(true);
  expect(typeof descriptor.value).toBe("function");
  expect(descriptor.writable).toBe(true);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});

test("a hole yields the value inherited from the prototype chain", () => {
  const arr = [1, , 3];
  expect([...arr.values()]).toEqual([1, undefined, 3]);
  Array.prototype[1] = "inherited";
  try {
    expect([...arr.values()]).toEqual([1, "inherited", 3]);
    const seen = [];
    for (const value of arr) {
      seen.push(value);
    }
    expect(seen).toEqual([1, "inherited", 3]);
  } finally {
    delete Array.prototype[1];
  }
});

test("an index defined as an accessor runs its getter at each step", () => {
  const arr = [1, 2, 3];
  let reads = 0;
  Object.defineProperty(arr, "1", {
    get() {
      reads += 1;
      return "computed";
    },
    configurable: true,
  });
  expect([...arr.values()]).toEqual([1, "computed", 3]);
  expect(reads).toBe(1);
});

test("elements appended or replaced during iteration are observed", () => {
  const arr = [1, 2, 3];
  const seen = [];
  for (const value of arr) {
    seen.push(value);
    if (value === 1) {
      arr[2] = "replaced";
      arr.push("appended");
    }
  }
  expect(seen).toEqual([1, 2, "replaced", "appended"]);
});

test("elements removed during iteration end it early", () => {
  const arr = [1, 2, 3, 4];
  const seen = [];
  for (const value of arr) {
    seen.push(value);
    if (value === 2) {
      arr.length = 2;
    }
  }
  expect(seen).toEqual([1, 2]);
});
