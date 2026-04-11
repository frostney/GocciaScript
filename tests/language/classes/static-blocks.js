describe("Class static blocks", () => {
  test("basic static block execution", () => {
    let executed = false;
    class Foo {
      static {
        executed = true;
      }
    }
    expect(executed).toBe(true);
  });

  test("multiple static blocks execute in source order", () => {
    const order = [];
    class Foo {
      static {
        order.push(1);
      }
      static {
        order.push(2);
      }
      static {
        order.push(3);
      }
    }
    expect(order).toEqual([1, 2, 3]);
  });

  test("this refers to the class constructor", () => {
    let captured;
    class Foo {
      static {
        captured = this;
      }
    }
    expect(captured).toBe(Foo);
  });

  test("can access static properties", () => {
    class Foo {
      static x = 10;
      static {
        this.y = this.x * 2;
      }
    }
    expect(Foo.x).toBe(10);
    expect(Foo.y).toBe(20);
  });

  test("can set static properties", () => {
    class Config {
      static {
        this.debug = false;
        this.version = "1.0";
      }
    }
    expect(Config.debug).toBe(false);
    expect(Config.version).toBe("1.0");
  });

  test("can call static methods", () => {
    class Foo {
      static helper() {
        return 42;
      }
      static {
        this.result = this.helper();
      }
    }
    expect(Foo.result).toBe(42);
  });

  test("variable declarations are scoped to the block", () => {
    class Foo {
      static {
        const x = 10;
        this.value = x;
      }
    }
    expect(Foo.value).toBe(10);
  });

  test("static blocks with try/catch", () => {
    class Foo {
      static {
        try {
          this.status = "ok";
        } catch (e) {
          this.status = "error";
        }
      }
    }
    expect(Foo.status).toBe("ok");
  });

  test("static blocks with inheritance", () => {
    const order = [];
    class Base {
      static {
        order.push("Base");
      }
    }
    class Child extends Base {
      static {
        order.push("Child");
      }
    }
    expect(order).toEqual(["Base", "Child"]);
  });

  test("class expression with static blocks", () => {
    const Foo = class {
      static {
        this.initialized = true;
      }
    };
    expect(Foo.initialized).toBe(true);
  });

  test("static blocks interleaved with static properties", () => {
    class Foo {
      static a = 1;
      static {
        this.aDoubled = this.a * 2;
      }
      static b = 2;
      static {
        this.bDoubled = this.b * 2;
      }
    }
    expect(Foo.a).toBe(1);
    expect(Foo.b).toBe(2);
    expect(Foo.aDoubled).toBe(2);
    expect(Foo.bDoubled).toBe(4);
  });

  test("empty static block is allowed", () => {
    class Foo {
      static {}
    }
    expect(Foo).toBeDefined();
  });

  test("static block can use closures", () => {
    let getter;
    class Foo {
      static {
        const secret = 42;
        getter = () => secret;
      }
    }
    expect(getter()).toBe(42);
  });

  test("static block with conditional logic", () => {
    const env = "production";
    class App {
      static {
        if (env === "production") {
          this.mode = "prod";
        } else {
          this.mode = "dev";
        }
      }
    }
    expect(App.mode).toBe("prod");
  });

  test("static block accessing class name via this", () => {
    class MyClass {
      static {
        this.className = "MyClass";
      }
    }
    expect(MyClass.className).toBe("MyClass");
  });

  test("multiple classes with independent static blocks", () => {
    class A {
      static {
        this.value = "A";
      }
    }
    class B {
      static {
        this.value = "B";
      }
    }
    expect(A.value).toBe("A");
    expect(B.value).toBe("B");
  });

  test("static block with for-of loop", () => {
    class Foo {
      static {
        this.items = [];
        for (const item of [1, 2, 3]) {
          this.items.push(item * 2);
        }
      }
    }
    expect(Foo.items).toEqual([2, 4, 6]);
  });

  test("static block can access methods defined before it", () => {
    class Foo {
      static greet(name) {
        return `Hello, ${name}`;
      }
      static {
        this.greeting = this.greet("World");
      }
    }
    expect(Foo.greeting).toBe("Hello, World");
  });

  test("static block can access private static fields", () => {
    let value;
    class Foo {
      static #secret = 99;
      static {
        value = this.#secret;
      }
    }
    expect(value).toBe(99);
  });

  test("static block can set private static fields", () => {
    let getter;
    class Foo {
      static #data;
      static {
        this.#data = "initialized";
        getter = () => this.#data;
      }
    }
    expect(getter()).toBe("initialized");
  });

  test("named class expression with static block", () => {
    const MyClass = class Named {
      static {
        this.value = 42;
      }
    };
    expect(MyClass.value).toBe(42);
  });
});
