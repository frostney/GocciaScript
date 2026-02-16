/*---
description: Multi-level class inheritance chains
features: [class-inheritance, extends, super]
---*/

describe("two-level inheritance", () => {
  test("three-class hierarchy", () => {
    class Animal {
      constructor(name) { this.name = name; }
      speak() { return this.name; }
    }
    class Dog extends Animal {
      constructor(name, breed) {
        super(name);
        this.breed = breed;
      }
      bark() { return this.name + " barks"; }
    }
    class Poodle extends Dog {
      constructor(name) {
        super(name, "poodle");
        this.groomed = false;
      }
    }
    const p = new Poodle("Max");
    expect(p.name).toBe("Max");
    expect(p.breed).toBe("poodle");
    expect(p.groomed).toBe(false);
    expect(p.speak()).toBe("Max");
    expect(p.bark()).toBe("Max barks");
  });

  test("instanceof through three levels", () => {
    class A { constructor() { this.a = 1; } }
    class B extends A { constructor() { super(); this.b = 2; } }
    class C extends B { constructor() { super(); this.c = 3; } }
    const c = new C();
    expect(c.a).toBe(1);
    expect(c.b).toBe(2);
    expect(c.c).toBe(3);
    expect(c instanceof C).toBe(true);
    expect(c instanceof B).toBe(true);
    expect(c instanceof A).toBe(true);
  });

  test("method override at each level", () => {
    class Base { greet() { return "base"; } }
    class Mid extends Base { greet() { return "mid"; } }
    class Leaf extends Mid { greet() { return "leaf"; } }
    expect(new Base().greet()).toBe("base");
    expect(new Mid().greet()).toBe("mid");
    expect(new Leaf().greet()).toBe("leaf");
  });

  test("super.method through chain", () => {
    class Base {
      describe() { return "base"; }
    }
    class Mid extends Base {
      describe() { return super.describe() + "+mid"; }
    }
    class Leaf extends Mid {
      describe() { return super.describe() + "+leaf"; }
    }
    expect(new Leaf().describe()).toBe("base+mid+leaf");
  });

  test("inherited method from grandparent", () => {
    class A { hello() { return "hello from A"; } }
    class B extends A {}
    class C extends B {}
    expect(new C().hello()).toBe("hello from A");
  });
});

describe("repeated instantiation", () => {
  test("multiple instances of deep hierarchy", () => {
    class Entity {
      constructor(id) { this.id = id; }
    }
    class Actor extends Entity {
      constructor(id, name) {
        super(id);
        this.name = name;
      }
    }
    class Player extends Actor {
      constructor(id, name, score) {
        super(id, name);
        this.score = score;
      }
    }
    const p1 = new Player(1, "Alice", 100);
    const p2 = new Player(2, "Bob", 200);
    const p3 = new Player(3, "Carol", 300);
    expect(p1.id).toBe(1);
    expect(p1.name).toBe("Alice");
    expect(p1.score).toBe(100);
    expect(p2.id).toBe(2);
    expect(p3.score).toBe(300);
    expect(p1 instanceof Player).toBe(true);
    expect(p1 instanceof Actor).toBe(true);
    expect(p1 instanceof Entity).toBe(true);
  });
});
