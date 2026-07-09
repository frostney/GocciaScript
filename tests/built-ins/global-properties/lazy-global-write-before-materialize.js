/*---
description: >
  Former data-format runtime globals are no longer preinstalled lazy
  descriptors. A user-created globalThis property with the same name stays an
  ordinary script property and does not expose the built-in parser namespace.
features: [global-properties, globalThis]
---*/

describe("former runtime module global names", () => {
  test("YAML is absent before user code creates a property", () => {
    expect("YAML" in globalThis).toBe(false);
    expect(typeof YAML).toBe("undefined");
  });

  test("a user-created YAML property stays ordinary data", () => {
    globalThis.YAML = 123;
    expect(globalThis.YAML).toBe(123);
    const desc = Object.getOwnPropertyDescriptor(globalThis, "YAML");
    expect(desc.value).toBe(123);
    expect(desc.enumerable).toBe(true);
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(typeof globalThis.YAML.parse).toBe("undefined");
  });
});
