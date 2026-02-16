/*---
description: Chained method calls on newly constructed instances
features: [class-declaration, new-expression]
---*/

describe("new Foo().method() chaining", () => {
  test("chained method call on new instance", () => {
    class Greeter {
      constructor(name) { this.name = name; }
      greet() { return "hello " + this.name; }
    }
    expect(new Greeter("world").greet()).toBe("hello world");
  });

  test("chained property access on new instance", () => {
    class Point {
      constructor(x, y) { this.x = x; this.y = y; }
    }
    expect(new Point(3, 4).x).toBe(3);
    expect(new Point(3, 4).y).toBe(4);
  });

  test("chained method returning value", () => {
    class Calculator {
      constructor(val) { this.val = val; }
      double() { return this.val * 2; }
      square() { return this.val * this.val; }
    }
    expect(new Calculator(5).double()).toBe(10);
    expect(new Calculator(5).square()).toBe(25);
  });

  test("chained getter access on new instance", () => {
    class Circle {
      constructor(r) { this.radius = r; }
      get diameter() { return this.radius * 2; }
    }
    expect(new Circle(7).diameter).toBe(14);
  });
});

describe("new Foo().method() with inheritance", () => {
  test("chained inherited method", () => {
    class Animal {
      constructor(name) { this.name = name; }
      speak() { return this.name + " speaks"; }
    }
    class Dog extends Animal {
      constructor(name) { super(name); }
      bark() { return this.name + " barks"; }
    }
    expect(new Dog("Rex").speak()).toBe("Rex speaks");
    expect(new Dog("Rex").bark()).toBe("Rex barks");
  });

  test("chained overridden method", () => {
    class Base {
      tag() { return "base"; }
    }
    class Derived extends Base {
      tag() { return "derived"; }
    }
    expect(new Base().tag()).toBe("base");
    expect(new Derived().tag()).toBe("derived");
  });
});

describe("new Foo() in expressions", () => {
  test("new in array literal", () => {
    class Val {
      constructor(v) { this.v = v; }
    }
    const arr = [new Val(1), new Val(2), new Val(3)];
    expect(arr.length).toBe(3);
    expect(arr[0].v).toBe(1);
    expect(arr[2].v).toBe(3);
  });

  test("new as function argument", () => {
    class Wrapper {
      constructor(val) { this.val = val; }
    }
    const unwrap = (w) => w.val;
    expect(unwrap(new Wrapper(42))).toBe(42);
  });

  test("new with chained method as argument", () => {
    class Builder {
      constructor(prefix) { this.prefix = prefix; }
      build(suffix) { return this.prefix + suffix; }
    }
    const result = [new Builder("hello ").build("world")];
    expect(result[0]).toBe("hello world");
  });
});
