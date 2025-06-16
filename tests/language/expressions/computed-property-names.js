/*---
description: Computed property names with complex expressions and edge cases
features: [computed-property-names, symbols, expressions]
---*/

test("computed property names from mathematical expressions", () => {
  const obj1 = {
    [1 + 1]: "addition",
    [5 - 2]: "subtraction",
    [2 * 3]: "multiplication",
    [10 / 2]: "division",
    [2 ** 3]: "exponentiation",
  };

  expect(obj1[2]).toBe("addition");
  expect(obj1[3]).toBe("subtraction");
  expect(obj1[6]).toBe("multiplication");
  expect(obj1[5]).toBe("division");
  expect(obj1[8]).toBe("exponentiation");

  // Complex mathematical expression
  const complexKey = 1 + 2 - (3 * 4) / 5;
  const obj2 = {
    [complexKey]: "complex math",
  };
  expect(obj2[complexKey]).toBe("complex math");
});

test("computed property names from string expressions", () => {
  const prefix = "test";
  const suffix = "Key";
  const obj = {
    [prefix + suffix]: "concatenated",
    ["dynamic" + (1 + 1)]: "string with number",
    [`template${42}`]: "template literal",
    [String(123)]: "string conversion",
  };

  expect(obj.testKey).toBe("concatenated");
  expect(obj["dynamic2"]).toBe("string with number");
  expect(obj.template42).toBe("template literal");
  expect(obj["123"]).toBe("string conversion");
});

test("computed property names with symbols", () => {
  const sym1 = Symbol("key1");
  const sym2 = Symbol.for("key2");
  const sym3 = Symbol.iterator;

  const obj = {
    [sym1]: "symbol value 1",
    [sym2]: "symbol value 2",
    [sym3]: "iterator symbol",
    [Symbol("local")]: "local symbol",
  };

  expect(obj[sym1]).toBe("symbol value 1");
  expect(obj[sym2]).toBe("symbol value 2");
  expect(obj[Symbol.for("key2")]).toBe("symbol value 2");
  expect(obj[sym3]).toBe("iterator symbol");
  expect(obj[Symbol.iterator]).toBe("iterator symbol");
});

test("computed property names with function calls", () => {
  const getKey = (n) => `key${n}`;
  const transform = (str) => str.toUpperCase();

  const obj = {
    [getKey(1)]: "first",
    [getKey(2)]: "second",
    [transform("hello")]: "uppercase",
    [Math.max(10, 20, 5)]: "max value",
  };

  expect(obj.key1).toBe("first");
  expect(obj.key2).toBe("second");
  expect(obj.HELLO).toBe("uppercase");
  expect(obj[20]).toBe("max value");
});

test("computed property names with conditional expressions", () => {
  const condition = true;
  const obj = {
    [condition ? "truthy" : "falsy"]: "conditional",
    [5 > 3 ? "greater" : "lesser"]: "comparison",
    [null ?? "default"]: "nullish coalescing",
  };

  expect(obj.truthy).toBe("conditional");
  expect(obj.greater).toBe("comparison");
  expect(obj.default).toBe("nullish coalescing");
});

test("computed property names with complex object access", () => {
  const config = {
    keys: {
      primary: "main",
      secondary: "alt",
    },
    getValue: (type) => `${type}_value`,
  };

  const obj = {
    [config.keys.primary]: "primary data",
    [config.keys.secondary]: "secondary data",
    [config.getValue("test")]: "generated value",
  };

  expect(obj.main).toBe("primary data");
  expect(obj.alt).toBe("secondary data");
  expect(obj.test_value).toBe("generated value");
});

test("computed property names evaluation order", () => {
  let evalOrder = [];

  const fn1 = () => {
    evalOrder.push("fn1");
    return "key1";
  };
  const fn2 = () => {
    evalOrder.push("fn2");
    return "key2";
  };
  const fn3 = () => {
    evalOrder.push("fn3");
    return "key3";
  };

  const obj = {
    [fn1()]: "value1",
    regular: "regular",
    [fn2()]: "value2",
    [fn3()]: "value3",
  };

  expect(evalOrder).toEqual(["fn1", "fn2", "fn3"]);
  expect(obj.key1).toBe("value1");
  expect(obj.key2).toBe("value2");
  expect(obj.key3).toBe("value3");
});
