/*---
description: Type annotations around enum declarations are parsed and ignored at runtime
features: [types-as-comments, enum-declaration]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("enum type annotations", () => {
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

  test("enum used with generic type annotation", () => {
    enum Status {
      Active = 1,
      Inactive = 0
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
      OK = 200,
      NotFound = 404
    }

    const response: HasStatus = { status: HttpStatus.OK };
    expect(response.status).toBe(200);
  });
});
