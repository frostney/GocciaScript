/*---
description: type and interface declarations are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("type alias declaration is skipped", () => {
  type Point = { x: number, y: number };
  const p = { x: 1, y: 2 };
  expect(p.x).toBe(1);
});

test("interface declaration is skipped", () => {
  interface Animal {
    name: string;
    speak(): string;
  }
  const a = { name: "cat", speak: () => "meow" };
  expect(a.speak()).toBe("meow");
});

test("interface with extends is skipped", () => {
  interface Shape {
    area(): number;
  }

  interface Circle extends Shape {
    radius: number;
  }

  const c = { radius: 5, area: () => 78.5 };
  expect(c.radius).toBe(5);
});

test("multiple type declarations", () => {
  type ID = string | number;
  type Name = string;
  type User = { id: ID, name: Name };

  const user = { id: 1, name: "Alice" };
  expect(user.name).toBe("Alice");
});

test("generic type alias", () => {
  type Container<T> = { value: T };
  const c = { value: 42 };
  expect(c.value).toBe(42);
});

test("interface with optional properties", () => {
  interface Config {
    host: string;
    port?: number;
    debug?: boolean;
  }

  const cfg = { host: "localhost" };
  expect(cfg.host).toBe("localhost");
});
