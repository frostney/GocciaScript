/*---
description: Prototype chain lookup and property shadowing
features: [prototype-chain, Object.create]
---*/

describe("prototype chain", () => {
  test("properties are looked up through the prototype chain", () => {
    const parent = { greeting: "hello" };
    const child = Object.create(parent);
    expect(child.greeting).toBe("hello");
  });

  test("own properties shadow prototype properties", () => {
    const parent = { x: 1 };
    const child = Object.create(parent);
    child.x = 2;
    expect(child.x).toBe(2);
    expect(parent.x).toBe(1);
  });

  test("multi-level prototype chain", () => {
    const grandparent = { level: "grandparent", shared: "from-gp" };
    const parent = Object.create(grandparent);
    parent.level = "parent";
    const child = Object.create(parent);
    child.level = "child";

    expect(child.level).toBe("child");
    expect(child.shared).toBe("from-gp");
  });

  test("Object.getPrototypeOf returns the prototype", () => {
    const proto = { a: 1 };
    const obj = Object.create(proto);
    expect(Object.getPrototypeOf(obj)).toBe(proto);
  });

  test("hasOwn distinguishes own from inherited", () => {
    const parent = { inherited: true };
    const child = Object.create(parent);
    child.own = true;

    expect(Object.hasOwn(child, "own")).toBe(true);
    expect(Object.hasOwn(child, "inherited")).toBe(false);
  });

  test("Object.keys only returns own enumerable properties", () => {
    const parent = { a: 1, b: 2 };
    const child = Object.create(parent);
    child.c = 3;

    const keys = Object.keys(child);
    expect(keys).toEqual(["c"]);
  });

  test("in operator checks entire prototype chain", () => {
    const parent = { inherited: true };
    const child = Object.create(parent);
    child.own = true;

    expect("own" in child).toBe(true);
    expect("inherited" in child).toBe(true);
    expect("nonexistent" in child).toBe(false);
  });

  test("delete only removes own properties", () => {
    const parent = { x: 1 };
    const child = Object.create(parent);
    child.x = 2;

    expect(child.x).toBe(2);
    delete child.x;
    expect(child.x).toBe(1);
  });

  test("prototype chain with methods", () => {
    const animal = {
      speak() {
        return "...";
      },
    };
    const dog = Object.create(animal);
    dog.speak = () => "woof";

    expect(dog.speak()).toBe("woof");
    expect(animal.speak()).toBe("...");
  });

  test("Object.create with null prototype", () => {
    const obj = Object.create(null);
    obj.key = "value";
    expect(obj.key).toBe("value");
    expect(Object.getPrototypeOf(obj)).toBeNull();
  });
});
