/*---
description: Type arguments on new expressions in TypeScript files are parsed and ignored at runtime
features: [types-as-comments]
---*/

describe("new expression type arguments in TypeScript", () => {
  test("constructs with an argument list", () => {
    const entries = new Map<string, number>([["answer", 42]]);

    expect(entries.get("answer")).toBe(42);
  });

  test("constructs without an argument list", () => {
    class Box<T> {
      value = 42;
    }

    const box = new Box<number>;

    expect(box.value).toBe(42);
  });

  test("closes nested type arguments with a shift token", () => {
    const nested = new Map<string, Array<number>>();
    const values = new Set<Array<number>>;

    nested.set("values", [1, 2, 3]);
    values.add([4]);

    expect(nested.get("values")[2]).toBe(3);
    expect([...values][0][0]).toBe(4);
  });

  test("failed constructor probes preserve a later successful generic constructor", () => {
    class Comparable {
      valueOf() {
        return 1;
      }
    }

    const result = new Comparable < new Comparable < new Comparable < new Comparable < new Comparable < 2;
    const values = new Set<Array<boolean>>;

    values.add([result]);

    expect(result).toBe(true);
    expect([...values][0][0]).toBe(true);
  });
});
