/*---
description: Type annotations on class members are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("class field type annotations", () => {
  class Point {
    x: number = 0;
    y: number = 0;

    constructor(x: number, y: number) {
      this.x = x;
      this.y = y;
    }
  }

  const p = new Point(3, 4);
  expect(p.x).toBe(3);
  expect(p.y).toBe(4);
});

test("class method return type", () => {
  class Calculator {
    add(a: number, b: number): number {
      return a + b;
    }
  }

  const calc = new Calculator();
  expect(calc.add(2, 3)).toBe(5);
});

test("class with implements clause", () => {
  class Dog implements Animal {
    name: string;
    constructor(name: string) {
      this.name = name;
    }
    speak(): string {
      return this.name + " barks";
    }
  }

  const d = new Dog("Rex");
  expect(d.speak()).toBe("Rex barks");
});

test("class with generic parameters", () => {
  class Box<T> {
    value: T;
    constructor(value: T) {
      this.value = value;
    }
    get(): T {
      return this.value;
    }
  }

  const box = new Box(42);
  expect(box.get()).toBe(42);
});

test("class with extends and implements", () => {
  class Base {
    kind: string = "base";
  }

  class Derived extends Base implements Serializable {
    serialize(): string {
      return this.kind;
    }
  }

  const d = new Derived();
  expect(d.kind).toBe("base");
  expect(d.serialize()).toBe("base");
});

test("access modifiers are parsed", () => {
  class Person {
    public name: string;
    protected age: number = 0;
    readonly id: number = 1;

    constructor(name: string) {
      this.name = name;
    }
  }

  const p = new Person("Alice");
  expect(p.name).toBe("Alice");
  expect(p.id).toBe(1);
});

test("class getter/setter with return types", () => {
  class Rectangle {
    #width: number = 0;
    #height: number = 0;

    constructor(w: number, h: number) {
      this.#width = w;
      this.#height = h;
    }

    get area(): number {
      return this.#width * this.#height;
    }

    set width(value: number): void {
      this.#width = value;
    }
  }

  const r = new Rectangle(3, 4);
  expect(r.area).toBe(12);
  r.width = 5;
  expect(r.area).toBe(20);
});

test("optional class fields", () => {
  class Config {
    name: string = "default";
    debug?: boolean;
  }

  const c = new Config();
  expect(c.name).toBe("default");
});
