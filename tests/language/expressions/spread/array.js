/*---
description: Spread syntax for arrays
features: [array-spread]
---*/

test("spread syntax for arrays at the start of an array", () => {
  const arr = [1, 2, 3];
  const newArr = [...arr, 4, 5, 6];
  expect(newArr).toEqual([1, 2, 3, 4, 5, 6]);
});

test("spread syntax for arrays at the end of an array", () => {
  const arr = [1, 2, 3];
  const newArr = [0, ...arr];
  expect(newArr).toEqual([0, 1, 2, 3]);
});

test("spread syntax for sparse arrays", () => {
  const arr = [1, , 3];
  const newArr = [...arr, 4, 5, 6];
  expect(newArr).toEqual([1, undefined, 3, 4, 5, 6]);
});

test("spread syntax for concatatening arrays", () => {
  const fruits = ["apple", "banana"];
  const vegetables = ["carrot", "broccoli"];
  const food = [...fruits, "orange", ...vegetables, "spinach"];
  expect(food).toEqual([
    "apple",
    "banana",
    "orange",
    "carrot",
    "broccoli",
    "spinach",
  ]);
});

test("spread syntax for shallow copying arrays", () => {
  const original = [1, 2, 3];
  const copy = [...original];
  expect(copy).toEqual([1, 2, 3]);
  expect(copy).not.toBe(original);

  const deepArray = [
    [1, 2, 3],
    [4, 5, 6],
  ];
  const shallowCopy = [...deepArray];
  expect(shallowCopy).toEqual([
    [1, 2, 3],
    [4, 5, 6],
  ]);
  expect(shallowCopy).not.toBe(deepArray);
  expect(shallowCopy[0]).toBe(deepArray[0]);
  expect(shallowCopy[1]).toBe(deepArray[1]);
});

test("spread to created nested arrays", () => {
  let arr1 = [1, 2];
  const arr2 = [3, 4];
  const nested = [...arr1, [...arr2, 5]];
  expect(nested).toEqual([1, 2, [3, 4, 5]]);
});

test("spread to flattens nested arrays", () => {
  const nested = [
    [1, 2],
    [3, 4],
  ];
  const flattened = [...nested[0], ...nested[1]];
  expect(flattened).toEqual([1, 2, 3, 4]);
});

test("spread with empty arrays are being flattened", () => {
  const arr = [...[]];
  expect(arr).toEqual([]);

  const arr2 = [...[1, 2, 3], ...[]];
  expect(arr2).toEqual([1, 2, 3]);
});

test("spread reads a hole through the prototype chain", () => {
  const sparse = [1, , 3];
  Array.prototype[1] = "inherited";
  let inherited;
  try {
    inherited = [...sparse];
  } finally {
    delete Array.prototype[1];
  }
  expect(inherited).toEqual([1, "inherited", 3]);
  const copy = [...sparse];
  expect(copy.length).toBe(3);
  expect(1 in copy).toBe(true);
  expect(copy[1]).toBe(undefined);
});

test("spread sees a source that a hole getter grows or shrinks", () => {
  const growing = [1, , 3];
  Object.defineProperty(Array.prototype, "1", {
    get() {
      growing.push(4);
      return "grew";
    },
    configurable: true,
  });
  let grown;
  try {
    grown = [...growing];
  } finally {
    delete Array.prototype[1];
  }
  expect(grown).toEqual([1, "grew", 3, 4]);

  const shrinking = [1, , 3, 4];
  Object.defineProperty(Array.prototype, "1", {
    get() {
      shrinking.length = 2;
      return "shrank";
    },
    configurable: true,
  });
  let shrunk;
  try {
    shrunk = [...shrinking];
  } finally {
    delete Array.prototype[1];
  }
  expect(shrunk).toEqual([1, "shrank"]);
});

test("spread calls an own index getter of the source", () => {
  const source = [1, 2, 3];
  let reads = 0;
  Object.defineProperty(source, "1", {
    get() {
      reads++;
      return "got";
    },
  });
  expect([...source]).toEqual([1, "got", 3]);
  expect(reads).toBe(1);
});

test("spread uses a replaced Array.prototype[Symbol.iterator]", () => {
  const original = Array.prototype[Symbol.iterator];
  Array.prototype[Symbol.iterator] = ({
    *values() {
      yield "replaced";
    },
  }).values;
  let replaced;
  try {
    replaced = [...[1, 2, 3]];
  } finally {
    Array.prototype[Symbol.iterator] = original;
  }
  expect(replaced).toEqual(["replaced"]);
  expect([...[1, 2, 3]]).toEqual([1, 2, 3]);
});

test("spread reads an accessor Array.prototype[Symbol.iterator] once per spread", () => {
  const original = Array.prototype[Symbol.iterator];
  let gets = 0;
  Object.defineProperty(Array.prototype, Symbol.iterator, {
    get() {
      gets++;
      return original;
    },
    configurable: true,
  });
  let spread;
  let getsDuringSpread;
  try {
    spread = [...[1, 2], ...[3]];
    getsDuringSpread = gets;
  } finally {
    Object.defineProperty(Array.prototype, Symbol.iterator, {
      value: original,
      writable: true,
      configurable: true,
    });
  }
  expect(spread).toEqual([1, 2, 3]);
  expect(getsDuringSpread).toBe(2);
});

test("spread uses an own Symbol.iterator of the source array", () => {
  const source = [1, 2, 3];
  source[Symbol.iterator] = ({
    *values() {
      yield "own";
    },
  }).values;
  expect([...source]).toEqual(["own"]);
});

test("spread uses the iterator of a replaced array prototype", () => {
  const source = [1, 2, 3];
  Object.setPrototypeOf(source, {
    *[Symbol.iterator]() {
      yield "proto";
    },
  });
  expect([...source]).toEqual(["proto"]);
});

test("spread of an Array subclass instance yields its elements", () => {
  class Numbers extends Array {}
  const numbers = Numbers.from([1, 2, 3]);
  expect([...numbers]).toEqual([1, 2, 3]);
});

test("spread copies every index up to a length set far past the elements", () => {
  const farLength = 2000000;
  const source = [1, 2];
  source.length = farLength;
  const copy = [...source];
  expect(copy.length).toBe(farLength);
  expect(copy[1]).toBe(2);
  expect(Object.hasOwn(copy, farLength - 1)).toBe(true);
  expect(copy[farLength - 1]).toBe(undefined);
});

test("spread reads an element stored far past the others", () => {
  const farLength = 2000000;
  const source = [1];
  source[farLength - 1] = "last";
  const copy = [0, ...source];
  expect(copy.length).toBe(farLength + 1);
  expect(copy[farLength]).toBe("last");
});

test("spread reads an inherited value past the stored elements", () => {
  const farLength = 2000000;
  const source = [1];
  source.length = farLength;
  Array.prototype[farLength - 1] = "inherited";
  let copy;
  try {
    copy = [...source];
  } finally {
    delete Array.prototype[farLength - 1];
  }
  expect(copy.length).toBe(farLength);
  expect(Object.hasOwn(copy, farLength - 1)).toBe(true);
  expect(copy[farLength - 1]).toBe("inherited");
});
