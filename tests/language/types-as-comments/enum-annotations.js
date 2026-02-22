/*---
description: Type annotations on and around enum declarations are parsed and ignored at runtime
features: [types-as-comments, enum-declaration]
---*/

const isGocciaScript = typeof GocciaScript !== "undefined";

describe.runIf(isGocciaScript)("enum type annotations", () => {
  test("type annotations on enum members", () => {
    enum Direction {
      Up: number = 0,
      Down: number = 1,
      Left: number = 2,
      Right: number = 3
    }

    expect(Direction.Up).toBe(0);
    expect(Direction.Down).toBe(1);
    expect(Direction.Left).toBe(2);
    expect(Direction.Right).toBe(3);
  });

  test("string type annotations on enum members", () => {
    enum Color {
      Red: string = "red",
      Green: string = "green",
      Blue: string = "blue"
    }

    expect(Color.Red).toBe("red");
    expect(Color.Green).toBe("green");
    expect(Color.Blue).toBe("blue");
  });

  test("mixed annotated and unannotated members", () => {
    enum Mixed {
      Code: number = 200,
      Message = "OK"
    }

    expect(Mixed.Code).toBe(200);
    expect(Mixed.Message).toBe("OK");
  });

  test("symbol type annotation on enum member", () => {
    enum Tokens {
      Alpha: symbol = Symbol("alpha"),
      Beta: symbol = Symbol("beta")
    }

    expect(typeof Tokens.Alpha).toBe("symbol");
    expect(typeof Tokens.Beta).toBe("symbol");
  });

  test("variable annotated with enum type", () => {
    enum Direction {
      Up = 0,
      Down = 1
    }

    const dir: Direction = Direction.Up;
    expect(dir).toBe(0);
  });

  test("parameter annotated with enum type", () => {
    enum Color {
      Red = "red",
      Green = "green"
    }

    const getName = (c: Color): string => {
      return c;
    };

    expect(getName(Color.Red)).toBe("red");
  });

  test("enum member initializer with typed expression", () => {
    const base: number = 10;

    enum Values {
      A: number = base,
      B: number = base + 1
    }

    expect(Values.A).toBe(10);
    expect(Values.B).toBe(11);
  });

  test("enum used with generic type annotation", () => {
    enum Status {
      Active: number = 1,
      Inactive: number = 0
    }

    const statuses: Array<number> = [Status.Active, Status.Inactive];
    expect(statuses.length).toBe(2);
    expect(statuses[0]).toBe(1);
  });

  test("enum alongside interface and type declarations", () => {
    interface HasStatus {
      status: number;
    }

    type StatusCode = number;

    enum HttpStatus {
      OK: number = 200,
      NotFound: number = 404
    }

    const response: HasStatus = { status: HttpStatus.OK };
    expect(response.status).toBe(200);
  });
});
