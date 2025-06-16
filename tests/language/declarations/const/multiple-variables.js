/*---
description: Const variable declarations, scoping, and immutability work correctly
features: [const, block-scope, immutability]
---*/

test("multiple const declarations with different types", () => {
  const a = 5;
  const myOtherVar = 10;
  const myOtherVar2 = 11;
  const stringVar = "hello";
  const boolVar = true;
  const objVar = { key: "value" };
  const arrVar = [1, 2, 3];
  const nullVar = null;
  const undefinedVar = undefined;

  expect(a).toBe(5);
  expect(myOtherVar).toBe(10);
  expect(myOtherVar2).toBe(11);
  expect(stringVar).toBe("hello");
  expect(boolVar).toBe(true);
  expect(objVar).toEqual({ key: "value" });
  expect(arrVar).toEqual([1, 2, 3]);
  expect(nullVar).toBe(null);
  expect(undefinedVar).toBe(undefined);

  // Test that variables maintain their types
  expect(typeof a).toBe("number");
  expect(typeof myOtherVar).toBe("number");
  expect(typeof myOtherVar2).toBe("number");
  expect(typeof stringVar).toBe("string");
  expect(typeof boolVar).toBe("boolean");
  expect(typeof objVar).toBe("object");
  expect(typeof arrVar).toBe("object");
  expect(typeof nullVar).toBe("object");
  expect(typeof undefinedVar).toBe("undefined");
});

test("const variable scoping and shadowing", () => {
  const outerVar = "outer";
  let results = [];

  // Test block scoping
  {
    const blockVar = "block";
    const outerVar = "shadowed"; // Shadows outer variable
    results.push(outerVar); // Should be "shadowed"
    results.push(blockVar);
  }

  results.push(outerVar); // Should still be "outer"

  expect(results).toEqual(["shadowed", "block", "outer"]);
});

test("const with complex objects and arrays", () => {
  const complexObj = {
    numbers: [1, 2, 3],
    nested: {
      prop: "value",
      func: function () {
        return this.prop;
      },
    },
    calculate: function (x, y) {
      return x + y;
    },
  };

  const complexArray = [
    { id: 1, name: "first" },
    { id: 2, name: "second" },
    [10, 20, 30],
  ];

  // Test object access
  expect(complexObj.numbers).toEqual([1, 2, 3]);
  expect(complexObj.nested.prop).toBe("value");
  expect(complexObj.nested.func()).toBe("value");
  expect(complexObj.calculate(5, 3)).toBe(8);

  // Test array access
  expect(complexArray[0]).toEqual({ id: 1, name: "first" });
  expect(complexArray[1].name).toBe("second");
  expect(complexArray[2]).toEqual([10, 20, 30]);
  expect(complexArray[2][1]).toBe(20);

  // Test that const prevents reassignment but allows mutation
  complexObj.newProp = "added";
  expect(complexObj.newProp).toBe("added");

  complexArray.push("new item");
  expect(complexArray).toHaveLength(4);
  expect(complexArray[3]).toBe("new item");
});

test("const with functions and closures", () => {
  const createCounter = function (initial) {
    const startValue = initial || 0;
    let count = startValue;

    const increment = function () {
      count = count + 1;
      return count;
    };

    const decrement = function () {
      count = count - 1;
      return count;
    };

    const get = function () {
      return count;
    };

    const reset = function () {
      count = startValue;
      return count;
    };

    return { increment, decrement, get, reset };
  };

  const counter1 = createCounter(5);
  const counter2 = createCounter(10);

  expect(counter1.get()).toBe(5);
  expect(counter2.get()).toBe(10);

  expect(counter1.increment()).toBe(6);
  expect(counter1.increment()).toBe(7);
  expect(counter2.decrement()).toBe(9);

  expect(counter1.reset()).toBe(5);
  expect(counter2.reset()).toBe(10);
});

test("const expressions and computations", () => {
  const base = 10;
  const multiplier = 3;
  const computed = base * multiplier + 5;
  const stringComputed = "Result: " + computed;
  const boolComputed = computed > 30;
  const arrayComputed = [base, multiplier, computed];
  const objComputed = {
    base: base,
    multiplier: multiplier,
    result: computed,
    formatted: stringComputed,
  };

  expect(computed).toBe(35);
  expect(stringComputed).toBe("Result: 35");
  expect(boolComputed).toBe(true);
  expect(arrayComputed).toEqual([10, 3, 35]);
  expect(objComputed).toEqual({
    base: 10,
    multiplier: 3,
    result: 35,
    formatted: "Result: 35",
  });
});

test("const with destructuring patterns", () => {
  const sourceArray = [1, 2, 3, 4, 5];
  const sourceObject = {
    name: "John",
    age: 30,
    address: { city: "New York", zip: "10001" },
  };

  // Array destructuring
  const [first, second, third, ...rest] = sourceArray;
  expect(first).toBe(1);
  expect(second).toBe(2);
  expect(third).toBe(3);
  expect(rest).toEqual([4, 5]);

  // Object destructuring
  const { name, age, address } = sourceObject;
  expect(name).toBe("John");
  expect(age).toBe(30);
  expect(address).toEqual({ city: "New York", zip: "10001" });

  // Nested destructuring
  const {
    address: { city, zip },
  } = sourceObject;
  expect(city).toBe("New York");
  expect(zip).toBe("10001");
});
