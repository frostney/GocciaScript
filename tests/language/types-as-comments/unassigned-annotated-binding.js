/*---
description: An annotated binding that holds a value no check has seen gives the generic operator result
features: [types-as-comments, strict-type-enforcement]
---*/

// ES2026 §14.3.1.2: a let without an initializer holds undefined, and
// §13.15.3 ApplyStringOrNumericBinaryOperator gives NaN for undefined + 1.

describe("annotated binding declared without an initializer", () => {
  test("adding a small integer gives NaN", () => {
    const run = () => {
      let value: number;
      return [value + 1, 1 + value];
    };

    expect(run()).toEqual([NaN, NaN]);
  });

  test("subtracting a small integer gives NaN", () => {
    const run = () => {
      let value: number;
      return value - 1;
    };

    expect(run()).toBeNaN();
  });

  test("a definite assignment assertion read before its assignment gives NaN", () => {
    const run = () => {
      let value!: number;
      return value + 1;
    };

    expect(run()).toBeNaN();
  });

  test("a closure reading the binding gives NaN", () => {
    const run = () => {
      let value: number;
      const next = () => value + 1;
      return next();
    };

    expect(run()).toBeNaN();
  });

  test("a const copied from the binding gives NaN", () => {
    const run = () => {
      let value: number;
      const copy = value;
      return copy + 1;
    };

    expect(run()).toBeNaN();
  });

  test("a function returning the binding gives NaN to its caller", () => {
    const read = () => {
      let value: number;
      return value;
    };

    expect(read() + 1).toBeNaN();
  });

  test("an assigned Number is added as a Number", () => {
    const run = () => {
      let value: number;
      value = 5;
      return value + 1;
    };

    expect(run()).toBe(6);
  });

  test("assigning a value of another type still throws", () => {
    const run = () => {
      let value: number;
      value = "text";
    };

    expect(run).toThrow(TypeError);
  });

  test("a compound addition gives NaN", () => {
    const run = () => {
      let value: number;
      value += 1;
      return value;
    };

    expect(run()).toBeNaN();
  });
});

describe("annotated rest parameter", () => {
  test("subtracting from the rest array gives NaN", () => {
    const run = (...values: number) => values - 1;

    expect(run(1, 2)).toBeNaN();
  });

  test("adding to the rest array concatenates its string form", () => {
    const run = (...values: number) => values + 1;

    expect(run(1, 2)).toBe("1,21");
  });

  test("assigning a value of another type still throws", () => {
    const run = (...values: number) => {
      values = "text";
    };

    expect(run).toThrow(TypeError);
  });

  test("a compound addition that concatenates throws", () => {
    const run = (...values: number) => {
      values += 1;
    };

    expect(() => run(1, 2)).toThrow(TypeError);
  });

  test("a compound addition in a closure that concatenates throws", () => {
    const run = (...values: number) => {
      const add = () => {
        values += 1;
      };
      add();
    };

    expect(() => run(1, 2)).toThrow(TypeError);
  });
});
