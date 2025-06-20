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
