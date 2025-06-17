/*---
description: Template literal interpolation support
features: [template-literals, template-interpolation]
---*/

test("template literal interpolation", () => {
  const name = "world";
  const greeting = `Hello ${name}!`;
  expect(greeting).toBe("Hello world!");

  const x = 5;
  const y = 10;
  const math = `${x} + ${y} = ${x + y}`;
  expect(math).toBe("5 + 10 = 15");
});
