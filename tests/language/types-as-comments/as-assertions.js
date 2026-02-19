/*---
description: as Type and as const assertions are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("as type assertion", () => {
  const x = 42 as number;
  expect(x).toBe(42);
});

test("as const assertion", () => {
  const x = 42 as const;
  expect(x).toBe(42);
});

test("as assertion in expression", () => {
  const arr = [1, 2, 3];
  const len = (arr as Array<number>).length;
  expect(len).toBe(3);
});

test("as assertion with string type", () => {
  const val = "hello" as string;
  expect(val).toBe("hello");
});

test("as const with array", () => {
  const colors = ["red", "green", "blue"] as const;
  expect(colors.length).toBe(3);
  expect(colors[0]).toBe("red");
});

test("as const with object", () => {
  const config = { port: 3000, host: "localhost" } as const;
  expect(config.port).toBe(3000);
});
