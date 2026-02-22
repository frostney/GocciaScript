/*---
description: Enum members can have string literal names
features: [enum-declaration]
---*/

test("string literal member names", () => {
  enum Named {
    Identifier = 0,
    "string name" = 1
  }

  expect(Named.Identifier).toBe(0);
  expect(Named["string name"]).toBe(1);
});

test("string names appear in iteration entries", () => {
  enum S {
    "hello world" = 42
  }

  const entries = [...S];
  expect(entries[0][0]).toBe("hello world");
  expect(entries[0][1]).toBe(42);
});
