/*---
description: Type arguments on new expressions are parsed and ignored at runtime
features: [types-as-comments]
---*/

describe("new expression type arguments", () => {
  test("constructs built-ins with erased type arguments", () => {
    const entries = new Map<string, string>([["name", "Goccia"]]);
    const values = new Set<number>([1, 2, 2]);

    expect(entries.get("name")).toBe("Goccia");
    expect(values.size).toBe(2);
  });

  test("supports nested and object type arguments", () => {
    const nested = new Map<string, Array<number>>();
    const records = new Set<{ value: number }>();

    nested.set("values", [1, 2, 3]);
    records.add({ value: 42 });

    expect(nested.get("values")[2]).toBe(3);
    expect([...records][0].value).toBe(42);
  });

  test("supports type arguments on member-expression constructors", () => {
    const entries = new globalThis.Map<string, number>([["answer", 42]]);

    expect(entries.get("answer")).toBe(42);
  });

  test("constructs without an explicit argument list", () => {
    class Box<T> {
      value = 42;
    }

    const box = new Box<number>;

    expect(box.value).toBe(42);
  });

  test("preserves relational expressions following new expressions", () => {
    class Comparable {
      valueOf() {
        return 1;
      }
    }

    expect(new Comparable < 2).toBe(true);
    expect(new Comparable < 2 > 0).toBe(true);
    expect(new Comparable() < 2).toBe(true);
  });
});
