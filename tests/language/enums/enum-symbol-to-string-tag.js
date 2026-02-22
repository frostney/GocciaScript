/*---
description: Enum objects have Symbol.toStringTag set to their name
features: [enum-declaration, symbol-to-string-tag]
---*/

test("Symbol.toStringTag matches enum name", () => {
  enum Direction {
    Up = 0,
    Down = 1
  }

  expect(Direction[Symbol.toStringTag]).toBe("Direction");
});

test("different enums have different toStringTag", () => {
  enum Alpha { A = 1 }
  enum Beta { B = 2 }

  expect(Alpha[Symbol.toStringTag]).toBe("Alpha");
  expect(Beta[Symbol.toStringTag]).toBe("Beta");
});
