/*---
description: >
  A script's top-level let and const binding patterns initialize each binding
  as its element completes (ES2026 §8.6.2 BindingInitialization, §8.6.3
  IteratorBindingInitialization, §14.3.3.3 KeyedBindingInitialization), so a
  later element's default reads an earlier binding of the same pattern, while
  a binding the pattern has not reached yet stays in its temporal dead zone.
features: [let, const-declaration, destructuring, temporal-dead-zone]
---*/

const errorNameOf = (read) => {
  try {
    read();
  } catch (error) {
    return error.constructor.name;
  }
  return "no error";
};

const [arrayFirst, arraySecond = arrayFirst] = [1];
let [letFirst, letSecond = letFirst] = [2];
const { objectFirst, objectSecond = objectFirst } = { objectFirst: 3 };
const { nestedFirst, nested: [nestedSecond = nestedFirst] } = {
  nestedFirst: 4,
  nested: [],
};
const [[innerFirst], { outerSecond = innerFirst * 2 }] = [[5], {}];
const [laterRead = () => laterValue, laterValue = 6] = [];
const [capturedValue, readCaptured = () => capturedValue] = [7];
const [forwardRead = errorNameOf(() => forwardTarget), forwardTarget] = [
  undefined,
  8,
];

describe("top-level lexical binding patterns", () => {
  test("an array pattern default reads an earlier const binding", () => {
    expect(arrayFirst).toBe(1);
    expect(arraySecond).toBe(1);
  });

  test("an array pattern default reads an earlier let binding", () => {
    expect(letFirst).toBe(2);
    expect(letSecond).toBe(2);
  });

  test("an object pattern default reads an earlier binding", () => {
    expect(objectFirst).toBe(3);
    expect(objectSecond).toBe(3);
  });

  test("a nested pattern default reads an earlier binding", () => {
    expect(nestedFirst).toBe(4);
    expect(nestedSecond).toBe(4);
    expect(innerFirst).toBe(5);
    expect(outerSecond).toBe(10);
  });

  test("closures created in a default read the final bindings", () => {
    expect(laterRead()).toBe(6);
    expect(readCaptured()).toBe(7);
  });

  test("a default that reads a later binding meets its dead zone", () => {
    expect(forwardRead).toBe("ReferenceError");
    expect(forwardTarget).toBe(8);
  });

  test("the bindings keep their declaration kind", () => {
    expect(() => {
      arraySecond = 0;
    }).toThrow(TypeError);
    expect(() => {
      nestedSecond = 0;
    }).toThrow(TypeError);
    letSecond = 20;
    expect(letSecond).toBe(20);
  });

  test("the bindings are not properties of the global object", () => {
    expect(Object.hasOwn(globalThis, "arraySecond")).toBe(false);
    expect(Object.hasOwn(globalThis, "letSecond")).toBe(false);
  });
});
