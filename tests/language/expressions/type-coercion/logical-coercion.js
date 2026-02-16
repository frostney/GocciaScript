/*---
description: Type coercion in logical and boolean operators
features: [type-coercion, logical]
---*/

describe("logical AND (&&) short-circuit", () => {
  test("returns first falsy value", () => {
    expect(false && "hello").toBe(false);
    expect(0 && "hello").toBe(0);
    expect("" && "hello").toBe("");
    expect(null && "hello").toBeNull();
    expect(undefined && "hello").toBe(undefined);
  });

  test("returns last value when all truthy", () => {
    expect(1 && 2).toBe(2);
    expect("a" && "b").toBe("b");
    expect(true && 42).toBe(42);
    expect(1 && 2 && 3).toBe(3);
  });
});

describe("logical OR (||) short-circuit", () => {
  test("returns first truthy value", () => {
    expect(false || "hello").toBe("hello");
    expect(0 || 42).toBe(42);
    expect("" || "default").toBe("default");
    expect(null || "fallback").toBe("fallback");
    expect(undefined || "backup").toBe("backup");
  });

  test("returns last value when all falsy", () => {
    expect(false || 0).toBe(0);
    expect(0 || "").toBe("");
    expect(null || undefined).toBe(undefined);
  });

  test("returns first truthy in chain", () => {
    expect(0 || "" || "found").toBe("found");
    expect(false || null || 42).toBe(42);
  });
});

describe("logical NOT (!)", () => {
  test("negates truthy values to false", () => {
    expect(!true).toBe(false);
    expect(!1).toBe(false);
    expect(!"hello").toBe(false);
    expect(![]).toBe(false);
    expect(!{}).toBe(false);
  });

  test("negates falsy values to true", () => {
    expect(!false).toBe(true);
    expect(!0).toBe(true);
    expect(!"").toBe(true);
    expect(!null).toBe(true);
    expect(!undefined).toBe(true);
    expect(!NaN).toBe(true);
  });

  test("double negation converts to boolean", () => {
    expect(!!1).toBe(true);
    expect(!!0).toBe(false);
    expect(!!"hello").toBe(true);
    expect(!!"").toBe(false);
    expect(!!null).toBe(false);
    expect(!!undefined).toBe(false);
  });
});
