/*---
description: Basic enum declaration and member access
features: [enum-declaration]
---*/

test("enum with number values", () => {
  enum Direction {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
  }

  expect(Direction.Up).toBe(0);
  expect(Direction.Down).toBe(1);
  expect(Direction.Left).toBe(2);
  expect(Direction.Right).toBe(3);
});

test("enum with string values", () => {
  enum Color {
    Red = "red",
    Green = "green",
    Blue = "blue"
  }

  expect(Color.Red).toBe("red");
  expect(Color.Green).toBe("green");
  expect(Color.Blue).toBe("blue");
});

test("enum with symbol values", () => {
  enum Tokens {
    Alpha = Symbol("alpha"),
    Beta = Symbol("beta")
  }

  expect(typeof Tokens.Alpha).toBe("symbol");
  expect(typeof Tokens.Beta).toBe("symbol");
  expect(Tokens.Alpha).not.toBe(Tokens.Beta);
});

test("typeof enum is object", () => {
  enum E { A = 1 }
  expect(typeof E).toBe("object");
});

test("enum with mixed number and string values", () => {
  enum Mixed {
    Code = 200,
    Message = "OK"
  }

  expect(Mixed.Code).toBe(200);
  expect(Mixed.Message).toBe("OK");
});

test("enum with trailing comma", () => {
  enum TrailingComma {
    A = 1,
    B = 2,
  }

  expect(TrailingComma.A).toBe(1);
  expect(TrailingComma.B).toBe(2);
});
