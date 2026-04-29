/*---
description: TC39 Pattern Matching expressions
features: [pattern-matching]
---*/

describe("pattern matching expressions", () => {
  test("is supports literals, wildcards, relational patterns, and logical patterns", () => {
    expect(1 is 1).toBe(true);
    expect(1 is 2).toBe(false);
    expect("x" is _).toBe(true);
    expect(3 is > 1 and < 5).toBe(true);
    expect(3 is 1 or 3).toBe(true);
    expect(3 is not 4).toBe(true);
  });

  test("is bindings are exposed only to the matched body", () => {
    const value = { x: 3 };
    let result = 0;

    if (value is { x: const x }) {
      result = x;
    }

    expect(result).toBe(3);
    expect(() => x).toThrow(ReferenceError);
  });

  test("body bindings work through required && conditions only", () => {
    const value = { x: 3 };
    let result = 0;

    if (value is { x: const x } && x > 2) {
      result = x;
    }

    expect(result).toBe(3);

    let hidden = false;
    try {
      if (value is { x: const y } || true) {
        y;
      }
    } catch (error) {
      hidden = error instanceof ReferenceError;
    }

    expect(hidden).toBe(true);
  });

  test("body bindings are cleaned up when required right conditions throw", () => {
    const value = { x: 3 };
    let caught = false;

    try {
      if (value is { x: const leaked } && (() => { throw new Error("boom"); })()) {
        leaked;
      }
    } catch (error) {
      caught = error.message === "boom";
    }

    expect(caught).toBe(true);
    expect(() => leaked).toThrow(ReferenceError);
  });

  test("ordinary and condition operands evaluate once", () => {
    let count = 0;
    const bump = () => {
      count = count + 1;
      return true;
    };

    if (bump() && true) {
      count = count + 1;
    }

    expect(count).toBe(2);
  });

  test("type assertions stop before a following is pattern", () => {
    expect(1 as number is 1).toBe(true);
  });

  test("type assertions require a type before is", () => {
    expect(() => new Function("const value = 1; return value as is 1;")).toThrow();
  });

  test("array and object patterns match nested values", () => {
    const value = { point: [2, 4, 6] };
    let sum = 0;

    if (value is { point: [const x, const y, ..._] }) {
      sum = x + y;
    }

    expect(sum).toBe(6);
  });

  test("object rest patterns expose unmatched properties", () => {
    const value = { a: 1, b: 2, c: 3 };
    let result = 0;

    if (value is { a: 1, ...const rest }) {
      result = rest.b + rest.c;
      if (rest.a !== undefined) {
        result = -1;
      }
    }

    expect(result).toBe(5);
  });

  test("object rest patterns exclude matched symbol properties", () => {
    const key = Symbol("key");
    const value = { [key]: 1, other: 2 };
    let result = 0;

    if (value is { [key]: const matched, ...const rest }) {
      result = matched + rest.other;
      if (rest[key] !== undefined) {
        result = -1;
      }
    }

    expect(result).toBe(3);
    expect({ a: 1, b: 2 } is { a: 1, ...{ b: 1 } }).toBe(false);
  });

  test("object rest patterns box primitive subjects", () => {
    let result = "";

    if ("ab" is { length: 2, ...const rest }) {
      result = rest[0] + rest[1];
    }

    expect(result).toBe("ab");
  });

  test("object patterns match symbol properties on boxed primitive prototypes", () => {
    const key = Symbol("boxed");

    try {
      String.prototype[key] = 7;
      expect("value" is { [key]: 7 }).toBe(true);
    } finally {
      Reflect.deleteProperty(String.prototype, key);
    }
  });

  test("object patterns consult proxy has traps", () => {
    const key = Symbol("virtual");
    const proxy = new Proxy({}, {
      has(target, property) {
        return property === "virtual" || property === key;
      },
      get(target, property) {
        if (property === "virtual") {
          return 7;
        }
        if (property === key) {
          return 3;
        }
      }
    });

    let result = 0;
    if (proxy is { virtual: 7, [key]: const value }) {
      result = value;
    }

    expect(result).toBe(3);
  });

  test("object rest patterns skip non-enumerable symbol properties", () => {
    const hidden = Symbol("hidden");
    const visible = Symbol("visible");
    const value = { [visible]: 2 };
    let result = 0;

    Object.defineProperty(value, hidden, { value: 1, enumerable: false });
    if (value is { ...const rest }) {
      result = rest[visible];
      if (rest[hidden] !== undefined) {
        result = -1;
      }
    }

    expect(result).toBe(2);
  });

  test("pattern binding names are case-sensitive", () => {
    const value = { foo: 1, Foo: 2 };
    let result = 0;

    if (value is { foo: const foo, Foo: const Foo }) {
      result = foo + Foo;
    }

    expect(result).toBe(3);
  });

  test("or pattern alternatives restore failed tentative bindings", () => {
    const chosen = "outer";
    const value = { bad: "bad", kind: "second" };
    let matched = false;

    if (value is { bad: const chosen, kind: "first" } or (_ if (chosen === "bad") as const chosen)) {
      matched = true;
    }

    expect(matched).toBe(false);
    expect(chosen).toBe("outer");
  });

  test("array patterns consume generic iterables and bind rest tails", () => {
    const iterable = {
      [Symbol.iterator]() {
        let value = 0;
        return {
          next() {
            value = value + 1;
            if (value > 3) {
              return { done: true };
            }
            return { value, done: false };
          }
        };
      }
    };

    let result = 0;
    if (iterable is [1, ...const tail]) {
      result = tail[0] + tail[1];
    }

    expect(result).toBe(5);
    expect({} is []).toBe(false);
    expect([0, 2] is [0, ...[1]]).toBe(false);
  });

  test("array patterns use the iterator protocol for arrays", () => {
    const values = [1, 2];
    values[Symbol.iterator] = () => {
      let done = false;
      return {
        next() {
          if (done) {
            return { done: true };
          }
          done = true;
          return { value: 9, done: false };
        }
      };
    };

    expect(values is [9]).toBe(true);
    expect(values is [1, 2]).toBe(false);
  });

  test("as bindings and guards commit only after success", () => {
    const value = { x: 4 };
    let result = 0;

    if (value is { x: const x } as const whole if (x > 3)) {
      result = x + whole.x;
    }

    expect(result).toBe(8);
    expect(() => whole).toThrow(ReferenceError);
  });

  test("constructor value patterns match instances", () => {
    class Box {}
    class Other {}

    const box = new Box();

    expect(box is Box).toBe(true);
    expect(box is Other).toBe(false);
    expect({} is Box).toBe(false);
  });

  test("constructor value patterns do not identify builtins by shadowed names", () => {
    class Array {}
    class Object {}

    expect([] is Array).toBe(false);
    expect({} is Object).toBe(false);
    expect(new Object() is Object).toBe(true);
  });

  test("value patterns support private member chains", () => {
    class Box {
      #secret = 7;

      matches(value) {
        return value is this.#secret;
      }
    }

    const box = new Box();
    expect(box.matches(7)).toBe(true);
    expect(box.matches(8)).toBe(false);
  });

  test("builtin primitive constructors match primitive values", () => {
    const sym = Symbol("sample");

    expect("sample" is String).toBe(true);
    expect(7 is Number).toBe(true);
    expect(true is Boolean).toBe(true);
    expect("sample" is Number).toBe(false);
    expect(7 is String).toBe(false);
    expect(false is String).toBe(false);
    expect(0n is BigInt).toBe(true);
    expect(1n is BigInt).toBe(true);
    expect(0 is BigInt).toBe(false);
    expect(1 is BigInt).toBe(false);
    expect(sym is Symbol).toBe(true);
    expect("sample" is Symbol).toBe(false);
  });

  test("malformed iterators throw during array pattern matching", () => {
    const malformed = {
      [Symbol.iterator]() {
        return 1;
      }
    };

    expect(() => malformed is []).toThrow(TypeError);
    expect(() => ({ [Symbol.iterator]: 1 } is [])).toThrow(TypeError);
    expect(() => ({ [Symbol.iterator]() { return {}; } } is [])).toThrow(TypeError);
    expect(() => ({ [Symbol.iterator]() { return { next: 1 }; } } is [])).toThrow(TypeError);
  });

  test("array patterns treat missing iterator values as undefined", () => {
    const iterable = {
      [Symbol.iterator]() {
        let count = 0;
        return {
          next() {
            count = count + 1;
            if (count > 1) {
              return { done: true };
            }
            return { done: false };
          }
        };
      }
    };

    expect(iterable is [undefined]).toBe(true);
  });

  test("ordinary match calls remain valid without a clause block", () => {
    const match = (value) => value + 1;
    expect(match(2)).toBe(3);
  });

  test("match evaluates the subject once and uses first successful clause", () => {
    let count = 0;
    let defaultRan = false;
    const next = () => {
      count = count + 1;
      return { type: "ok", value: 7 };
    };

    const result = match (next()) {
      { type: "missing" }: 0;
      { type: "ok", value: const value }: value * 2;
      default: (() => { defaultRan = true; return -1; })();
    };

    expect(result).toBe(14);
    expect(count).toBe(1);
    expect(defaultRan).toBe(false);
    expect(() => value).toThrow(ReferenceError);
  });

  test("match throws TypeError when no clause matches", () => {
    expect(() => match (2) { 1: "one"; }).toThrow(TypeError);
  });
});
