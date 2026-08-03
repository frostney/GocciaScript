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

  test("keeps satisfies available as an identifier and property name", () => {
    const satisfies = (value) => value;
    const object = { satisfies: "property" };

    expect(satisfies("identifier")).toBe("identifier");
    expect(object.satisfies).toBe("property");
  });
});
