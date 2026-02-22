/*---
description: Enum objects support Symbol.iterator yielding [key, value] entries
features: [enum-declaration, symbol-iterator]
---*/

test("enum is iterable via Symbol.iterator", () => {
  enum Direction {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
  }

  expect(typeof Direction[Symbol.iterator]).toBe("function");
});

test("spread operator on enum yields entries", () => {
  enum Status {
    Active = 1,
    Inactive = 0
  }

  const entries = [...Status];
  expect(entries.length).toBe(2);
});

test("entries are [key, value] pairs", () => {
  enum Color {
    Red = "red",
    Blue = "blue"
  }

  const entries = [...Color];
  expect(entries[0][0]).toBe("Red");
  expect(entries[0][1]).toBe("red");
  expect(entries[1][0]).toBe("Blue");
  expect(entries[1][1]).toBe("blue");
});

test("entries preserve declaration order", () => {
  enum Order {
    C = 3,
    A = 1,
    B = 2
  }

  const entries = [...Order];
  expect(entries[0][0]).toBe("C");
  expect(entries[1][0]).toBe("A");
  expect(entries[2][0]).toBe("B");
});

test("Array.from works on enum", () => {
  enum Nums {
    X = 10,
    Y = 20
  }

  const arr = Array.from(Nums);
  expect(arr.length).toBe(2);
  expect(arr[0][0]).toBe("X");
  expect(arr[0][1]).toBe(10);
});

test("destructuring entries in for-of", () => {
  enum Size {
    Small = "S",
    Medium = "M",
    Large = "L"
  }

  const keys = [];
  const values = [];
  const entries = [...Size];
  entries.forEach(([key, value]) => {
    keys.push(key);
    values.push(value);
  });

  expect(keys.length).toBe(3);
  expect(keys[0]).toBe("Small");
  expect(values[0]).toBe("S");
});
