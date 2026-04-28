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

  test("type assertions stop before a following is pattern", () => {
    expect(1 as number is 1).toBe(true);
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

  test("object rest patterns box primitive subjects", () => {
    let result = "";

    if ("ab" is { length: 2, ...const rest }) {
      result = rest[0] + rest[1];
    }

    expect(result).toBe("ab");
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

    expect([] is Array).toBe(false);
  });

  test("malformed iterators throw during array pattern matching", () => {
    const malformed = {
      [Symbol.iterator]() {
        return 1;
      }
    };

    expect(() => malformed is []).toThrow(TypeError);
    expect(() => ({ [Symbol.iterator]: 1 } is [])).toThrow(TypeError);
  });

  test("ordinary match calls remain valid without a clause block", () => {
    const match = (value) => value + 1;
    expect(match(2)).toBe(3);
  });

  test("match evaluates the subject once and uses first successful clause", () => {
    let count = 0;
    const next = () => {
      count = count + 1;
      return { type: "ok", value: 7 };
    };

    const result = match (next()) {
      { type: "missing" }: 0;
      { type: "ok", value: const value }: value * 2;
      default: -1;
    };

    expect(result).toBe(14);
    expect(count).toBe(1);
    expect(() => value).toThrow(ReferenceError);
  });

  test("match throws TypeError when no clause matches", () => {
    expect(() => match (2) { 1: "one"; }).toThrow(TypeError);
  });
});
