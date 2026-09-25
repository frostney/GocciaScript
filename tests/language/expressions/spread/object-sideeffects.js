/*---
description: Spread syntax for objects with side effects
features: [object-sideeffects]
---*/

test("spread evaluation order with side effects", () => {
  let sideEffects = [];

  const obj1 = {
    get a() {
      sideEffects.push("get-a");
      return 1;
    },
  };

  const obj2 = {
    get b() {
      sideEffects.push("get-b");
      return 2;
    },
  };

  const result = {
    start: (() => {
      sideEffects.push("start");
      return 0;
    })(),
    ...obj1,
    middle: (() => {
      sideEffects.push("middle");
      return 1.5;
    })(),
    ...obj2,
    end: (() => {
      sideEffects.push("end");
      return 3;
    })(),
  };

  expect(result).toEqual({ start: 0, a: 1, middle: 1.5, b: 2, end: 3 });
  expect(sideEffects).toEqual(["start", "get-a", "middle", "get-b", "end"]);
});

test("spread reads index keys before other strings and then symbols", () => {
  const reads = [];
  const symbol = Symbol("last");
  const source = {};
  for (const key of ["4294967295", "01", "4294967294", "2", symbol, "0"]) {
    Object.defineProperty(source, key, {
      enumerable: true,
      get: () => { reads.push(key); return key; },
    });
  }
  const copy = { ...source };
  expect(reads).toEqual(["0", "2", "4294967294", "4294967295", "01", symbol]);
  expect(copy[symbol]).toBe(symbol);
});

test("spread includes redefined enumerable class intrinsic properties", () => {
  const reads = [];
  class Source { static value = 1; }
  for (const key of ["name", "length"]) {
    Object.defineProperty(Source, key, {
      configurable: true,
      enumerable: true,
      get: () => { reads.push(key); return key; },
    });
  }
  const copy = { ...Source };
  expect(reads).toEqual(["length", "name"]);
  expect(copy).toEqual({ length: "length", name: "name", value: 1 });
});
