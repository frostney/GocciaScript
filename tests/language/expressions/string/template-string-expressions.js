/*---
description: Template literal complete expression support
features: [template-literals, full-expressions]
---*/

test("template literal with expressions", () => {
  // Basic variable interpolation
  const name = "world";
  const greeting = `Hello ${name}!`;
  expect(greeting).toBe("Hello world!");

  // Arithmetic expressions
  const x = 5;
  const y = 10;
  const math = `${x} + ${y} = ${x + y}`;
  expect(math).toBe("5 + 10 = 15");

  // String concatenation in templates
  const first = "John";
  const last = "Doe";
  const fullName = `Full name: ${first + " " + last}`;
  expect(fullName).toBe("Full name: John Doe");

  // Conditional expressions
  const age = 25;
  const status = `You are ${age >= 18 ? "an adult" : "a minor"}`;
  expect(status).toBe("You are an adult");

  // Object property access
  const person = { name: "Alice", age: 30 };
  const info = `${person.name} is ${person.age} years old`;
  expect(info).toBe("Alice is 30 years old");

  // Function calls
  const numbers = [1, 2, 3];
  const arrayInfo = `Array length: ${numbers.length}`;
  expect(arrayInfo).toBe("Array length: 3");

  // Nested expressions
  const a = 2;
  const b = 3;
  const complex = `Result: ${(a + b) * 2}`;
  expect(complex).toBe("Result: 10");
});
