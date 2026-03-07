/*---
description: Strict type inference — initializer locks variable type, union/any/unknown remain untyped
features: [types-as-comments]
---*/

describe.skipIf(!GocciaScript.strictTypes)("strict type inference", () => {
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

describe.skipIf(!GocciaScript.strictTypes)("typed uninitialized enforcement", () => {
  test("let without initializer enforces type on subsequent assignment", () => {
    let x: number;
    x = 42;
    expect(x).toBe(42);
    expect(() => { x = "hello"; }).toThrow(TypeError);
  });
});
