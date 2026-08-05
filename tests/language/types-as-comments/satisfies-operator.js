/*---
description: TypeScript satisfies suffixes are parsed and ignored at runtime
features: [types-as-comments]
---*/

type Config = { port: number };

describe("satisfies suffix", () => {
  test("preserves the direct expression value", () => {
    const config = { port: 3000 } satisfies Config;

    expect(config.port).toBe(3000);
  });

  test("follows an as const assertion", () => {
    const config = { port: 3000 } as const satisfies Config;

    expect(config.port).toBe(3000);
  });

  test("supports nested generic and object types", () => {
    const values = [1, 2, 3] satisfies Array<number>;
    const record = { value: 42 } satisfies { value: number };

    expect(values[2]).toBe(3);
    expect(record.value).toBe(42);
  });

  test("does not perform static type checking", () => {
    const unchecked = "text" satisfies number;

    expect(unchecked).toBe("text");
  });

  test("supports chained as and satisfies suffixes", () => {
    const asserted = "value" as string as unknown;
    const checked = "value" as string satisfies unknown;

    expect(asserted).toBe("value");
    expect(checked).toBe("value");
  });

  test("keeps satisfies available as an identifier and property name", () => {
    const satisfies = (value) => value;
    const object = { satisfies: "property" };

    const asserted = "value" as satisfies;

    expect(satisfies("identifier")).toBe("identifier");
    expect(object.satisfies).toBe("property");
    expect(asserted).toBe("value");
  });
});
