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

test("an object type in a type parameter's default or constraint is not the body", () => {
  interface Box<T = {}> {
    contents: T;
  }
  interface Crate<T extends { id: string } = { id: string }> extends Box<T> {
    label: string;
  }

  const crate = { contents: { id: "c-1" }, label: "fragile" };
  expect(crate.label).toBe("fragile");
});

test("an object type in an extends clause's type arguments is not the body", () => {
  interface Base<T> {
    item: T;
  }
  interface Shelf extends Base<{ id: string }>, Base<{ size: number }> {
    count: number;
  }

  const shelf = { item: { id: "s-1" }, count: 2 };
  expect(shelf.count).toBe(2);
});

test("a template literal type in an interface does not end it early", () => {
  interface Tagged<K extends `tag-${string}` = `tag-${string}`> {
    key: K;
    describe(id: `item-${number}`): `${string}!`;
  }

  const tagged = { key: "tag-a", describe: (id) => `${id}!` };
  expect(tagged.describe("item-1")).toBe("item-1!");
});
