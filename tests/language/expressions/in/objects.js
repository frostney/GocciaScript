test("in operator with objects", () => {
  const obj = {
    name: "test",
    age: 25,
    "special-key": "value",
    0: "zero",
    1: "one",
  };

  // String properties
  expect("name" in obj).toBe(true);
  expect("age" in obj).toBe(true);
  expect("special-key" in obj).toBe(true);
  expect("missing" in obj).toBe(false);

  // Numeric properties (as strings and numbers)
  expect("0" in obj).toBe(true);
  expect(0 in obj).toBe(true);
  expect("1" in obj).toBe(true);
  expect(1 in obj).toBe(true);
  expect("2" in obj).toBe(false);
  expect(2 in obj).toBe(false);

  // Empty string
  expect("" in obj).toBe(false);
});

test("in operator with objects with prototype", () => {
  const obj = Object.create({
    name: "test",
    age: 25,
  });

  expect("name" in obj).toBe(true);
  expect("age" in obj).toBe(true);
  expect("missing" in obj).toBe(false);
});

test("in operator with computed properties access in objects", () => {
  const obj = {
    name: "test",
    age: 25,
  };

  const computed = "name";
  expect(computed in obj).toBe(true);

  const computed2 = "age";
  expect(computed2 in obj).toBe(true);

  const computed3 = "missing";
  expect(computed3 in obj).toBe(false);
});

test("in operator in empty object", () => {
  const obj = {};
  expect("anything" in obj).toBe(false);
});

test("in operator with symbol keys", () => {
  const sym = Symbol("key");
  const other = Symbol("other");
  const obj = { [sym]: "value" };
  expect(sym in obj).toBe(true);
  expect(other in obj).toBe(false);
});

test("in operator with symbol keys in arrays", () => {
  const sym = Symbol("tag");
  const arr = [1, 2, 3];
  arr[sym] = "tagged";
  expect(sym in arr).toBe(true);
  expect(Symbol("missing") in arr).toBe(false);
});

test("in operator with Symbol.for keys", () => {
  const sym = Symbol.for("global");
  const obj = { [sym]: "registered" };
  expect(Symbol.for("global") in obj).toBe(true);
  expect(Symbol("global") in obj).toBe(false);
});
