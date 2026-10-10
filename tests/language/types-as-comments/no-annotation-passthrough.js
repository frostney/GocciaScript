/*---
description: Strict type inference — initializer locks variable type, union/any/unknown remain untyped
features: [types-as-comments]
---*/

describe("strict type inference", () => {
  test("inferred type prevents incompatible reassignment", () => {
    let x = 5;
    expect(x).toBe(5);
    expect(() => { x = "hello"; }).toThrow(TypeError);
    expect(() => { x = true; }).toThrow(TypeError);
  });

  test("inferred string type prevents number reassignment", () => {
    let s = "hello";
    expect(s).toBe("hello");
    expect(() => { s = 42; }).toThrow(TypeError);
  });

  test("inferred boolean type prevents number reassignment", () => {
    let b = true;
    expect(b).toBe(true);
    expect(() => { b = 1; }).toThrow(TypeError);
  });

  test("null initializer remains untyped", () => {
    let x = null;
    x = 5;
    expect(x).toBe(5);
    x = "hello";
    expect(x).toBe("hello");
  });

  test("undefined initializer remains untyped", () => {
    let x = undefined;
    x = 5;
    expect(x).toBe(5);
  });

  test("template literal initializer infers string", () => {
    const name = "world";
    let plain = `hello`;
    let interpolated = `hello ${name}`;
    expect(() => { plain = 1; }).toThrow(TypeError);
    expect(() => { interpolated = 1; }).toThrow(TypeError);
  });

  test("object, array, new and function initializers infer object", () => {
    let record = {};
    let list = [];
    let map = new Map();
    let callback = () => 1;
    expect(() => { record = 1; }).toThrow(TypeError);
    expect(() => { list = "text"; }).toThrow(TypeError);
    expect(() => { map = true; }).toThrow(TypeError);
    expect(() => { callback = 1; }).toThrow(TypeError);
    record = [];
    expect(Array.isArray(record)).toBe(true);
  });
});

describe("initializers that infer no type", () => {
  const base = 16;
  const double = (value: number): number => value * 2;

  test("const reference remains untyped", () => {
    let count = base;
    count = "text";
    expect(count).toBe("text");
  });

  test("let reference remains untyped", () => {
    let source = 1;
    let count = source;
    count = "text";
    expect(count).toBe("text");
    expect(() => { source = "text"; }).toThrow(TypeError);
  });

  test("arithmetic expression remains untyped", () => {
    let count = base + 1;
    count = "text";
    expect(count).toBe("text");

    let sum = 1 + 2;
    sum = "text";
    expect(sum).toBe("text");
  });

  test("negated number remains untyped", () => {
    let count = -1;
    count = "text";
    expect(count).toBe("text");
  });

  test("comparison remains untyped", () => {
    let flag = base > 1;
    flag = 1;
    expect(flag).toBe(1);
  });

  test("conditional expression remains untyped", () => {
    let count = base > 1 ? 1 : 2;
    count = "text";
    expect(count).toBe("text");
  });

  test("call with an annotated return type remains untyped", () => {
    let count = double(2);
    count = "text";
    expect(count).toBe("text");
  });

  test("captured binding remains untyped", () => {
    let count = base + 1;
    const assign = (next) => { count = next; };
    assign("4");
    expect(count + 1).toBe("41");
  });
});

describe("untyped let holding a value of another type", () => {
  const base = 16;
  const measure = () => 17;

  test("arithmetic, concatenation and comparison after reassignment", () => {
    let value = base + 1;
    expect(value + 1).toBe(18);
    value = "7";
    expect(value + 1).toBe("71");
    expect(value + 2.5).toBe("72.5");
    expect(value - 1).toBe(6);
    expect(value * 2.5).toBe(17.5);
    expect(value + "!").toBe("7!");
    expect(value < 10).toBe(true);
    expect(value < "10").toBe(false);
    expect(value === "7").toBe(true);
  });

  test("reads in a loop see the value assigned by the previous iteration", () => {
    let value = base + 1;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(value + 1, value + 2.5, value * 2, value < 5, value + "!");
      value = "4";
    }
    expect(seen).toEqual([18, 19.5, 34, false, "17!", "41", "42.5", 8, true, "4!"]);
  });

  test("number assigned before a loop does not type later reads", () => {
    let value = measure();
    value = 5;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(value + 1, value * 2, value < 5);
      value = "4";
    }
    expect(seen).toEqual([6, 10, false, "41", 8, true]);
  });

  test("reads after a switch see the branch that ran", () => {
    const pick = (key) => {
      let value = base + 1;
      switch (key) {
        case "text":
          value = "4";
          break;
        default:
          value = 3;
      }
      return [value + 1, value * 2, value < 4];
    };
    expect(pick("text")).toEqual(["41", 8, false]);
    expect(pick("number")).toEqual([4, 6, true]);
  });

  test("self-increment and compound assignment in a loop", () => {
    let value = base + 1;
    const seen = [];
    for (const step of [0, 1]) {
      value = value + 1;
      seen.push(value);
      value += 2;
      seen.push(value);
      value = "4";
    }
    expect(seen).toEqual([18, 20, "41", "412"]);
  });

  test("string and boolean assignments before a loop do not type later reads", () => {
    let text = measure();
    text = "a";
    let flag = measure();
    flag = true;
    const seen = [];
    for (const step of [0, 1]) {
      seen.push(typeof (text + ""), !!flag);
      text = 5;
      flag = 0;
    }
    expect(seen).toEqual(["string", true, "string", false]);
  });
});

// TC39 "Types as Comments" — union/any/unknown annotations are parsed but not enforced at runtime
test("union type annotation does not enforce", () => {
  let value: string | number = "hello";
  expect(value).toBe("hello");
  value = 42;
  expect(value).toBe(42);
  value = true;
  expect(value).toBe(true);
});

test("any type annotation does not enforce", () => {
  let value: any = 1;
  expect(value).toBe(1);
  value = "text";
  expect(value).toBe("text");
});

test("unknown type annotation does not enforce", () => {
  let value: unknown = 1;
  expect(value).toBe(1);
  value = "text";
  expect(value).toBe("text");
});

test("let without initializer with type allows first assignment", () => {
  let x: number;
  expect(x).toBe(undefined);
  x = 42;
  expect(x).toBe(42);
});

describe("typed uninitialized enforcement", () => {
  test("let without initializer enforces type on subsequent assignment", () => {
    let x: number;
    x = 42;
    expect(x).toBe(42);
    expect(() => { x = "hello"; }).toThrow(TypeError);
  });
});
