/*---
description: Public properties named like the engine's private storage keys stay ordinary properties and never reach private elements (#1326)
features: [class-fields-private, class-methods-private]
---*/

describe("properties named like private storage keys", () => {
  test("a string key does not read or write a private field", () => {
    class Box {
      #value = 1;
      read() {
        return this["#slot:0$value"];
      }
      write() {
        this["#slot:0$value"] = 99;
        return this.#value;
      }
    }
    const box = new Box();
    expect(box.read()).toBeUndefined();
    expect(box.write()).toBe(1);
    expect(box.read()).toBe(99);
    expect(Object.keys(box)).toEqual(["#slot:0$value"]);
  });

  test("a computed string key does not read a private field", () => {
    const key = "#slot:0$value";
    class Box {
      #value = 7;
      read() {
        return this[key];
      }
    }
    expect(new Box().read()).toBeUndefined();
  });

  test("a public field and a private field with matching storage text coexist", () => {
    class Pair {
      "#slot:0$z" = "public";
      #z = "private";
      both() {
        return [this["#slot:0$z"], this.#z];
      }
    }
    const pair = new Pair();
    expect(pair.both()).toEqual(["public", "private"]);
    expect(Object.keys(pair)).toEqual(["#slot:0$z"]);
  });

  test("a public method named like a private storage key keeps its name", () => {
    class Named {
      "#slot:0$m"() {
        return "public";
      }
      #m() {
        return "private";
      }
      callPrivate() {
        return this.#m();
      }
    }
    const named = new Named();
    expect(named["#slot:0$m"]()).toBe("public");
    expect(named["#slot:0$m"].name).toBe("#slot:0$m");
    expect(named.callPrivate()).toBe("private");
    expect(Object.getOwnPropertyNames(Named.prototype)).toEqual([
      "constructor",
      "#slot:0$m",
      "callPrivate",
    ]);
  });

  test("private elements stamped on a returned object stay hidden", () => {
    class Base {
      constructor(target) {
        return target;
      }
    }
    class Stamped extends Base {
      #field = 1;
      #method() {
        return this.#field;
      }
      static read(value) {
        return value.#method();
      }
    }
    const target = { "#slot:own": 1 };
    new Stamped(target);
    expect(Stamped.read(target)).toBe(1);
    expect(Object.keys(target)).toEqual(["#slot:own"]);
    expect(Reflect.ownKeys(target)).toEqual(["#slot:own"]);
  });
});
