/*---
description: Pattern matching keeps scopes and subjects alive across collections
features: [pattern-matching, Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

describe.runIf(hasGoccia)("pattern matching GC safety", () => {
  test("a closure escaping a match clause survives collection", () => {
    const make = (value) =>
      match (value) {
        { x: const x }: () => x;
        default: () => -1;
      };

    const escaped = make({ x: 42 });
    Goccia.gc();
    Goccia.gc();

    expect(escaped()).toBe(42);
  });

  test("a class escaping a match clause keeps its definition scope", () => {
    const makeClass = (value) =>
      match (value) {
        { x: const x }: class {
          field = x;
          read() {
            return x;
          }
        };
        default: null;
      };

    const Escaped = makeClass({ x: 7 });
    Goccia.gc();
    Goccia.gc();

    const instance = new Escaped();
    expect(instance.field).toBe(7);
    expect(instance.read()).toBe(7);
  });

  test("a closure escaping an is-guard body survives collection", () => {
    const value = { y: 11 };
    let escaped = null;

    if (value is { y: const y }) {
      escaped = () => y;
    }
    Goccia.gc();
    Goccia.gc();

    expect(escaped()).toBe(11);
  });

  test("a closure escaping an or-branch binding survives collection", () => {
    const make = (value) =>
      match (value) {
        { a: const bound } or { b: const bound }: () => bound;
        default: () => -1;
      };

    const escaped = make({ b: 5 });
    Goccia.gc();
    Goccia.gc();

    expect(escaped()).toBe(5);
  });

  test("a closure escaping a catch pattern body survives collection", () => {
    let escaped = null;

    try {
      throw { code: 3 };
    } catch (error is { code: const code }) {
      escaped = () => code;
    }
    Goccia.gc();
    Goccia.gc();

    expect(escaped()).toBe(3);
  });

  test("collecting inside a custom matcher leaves the match intact", () => {
    class Positive {
      static [Symbol.customMatcher](subject) {
        Goccia.gc();
        return subject > 0;
      }
    }

    const result = match (5) {
      Positive: "positive";
      default: "other";
    };

    expect(result).toBe("positive");
  });

  test("collecting inside a computed pattern key keeps the subject alive", () => {
    const key = () => {
      Goccia.gc();
      return "x";
    };

    const result = match ({ x: 1, z: 2 }) {
      { [key()]: const x, z: const z }: x + z;
      default: -1;
    };

    expect(result).toBe(3);
  });

  test("collecting inside a pattern guard keeps the subject alive", () => {
    const check = (value) => {
      Goccia.gc();
      return value > 0;
    };

    const result = match ({ x: 1 }) {
      { x: const x } if (check(x)): x;
      default: -1;
    };

    expect(result).toBe(1);
  });

  test("collecting inside an is-expression pattern keeps the subject alive", () => {
    const key = () => {
      Goccia.gc();
      return "x";
    };
    let result = 0;

    if ({ x: 9 } is { [key()]: const x }) {
      result = x;
    }

    expect(result).toBe(9);
  });
});
