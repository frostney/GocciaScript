/*---
description: Nullish coalescing operator (??) returns right-hand side for null/undefined only
features: [nullish-coalescing]
---*/

test("returns right-hand side for null", () => {
  const result = null ?? "default";
  expect(result).toBe("default");
});

test("returns right-hand side for undefined", () => {
  const result = undefined ?? "default";
  expect(result).toBe("default");
});

test("returns left-hand side for zero", () => {
  const result = 0 ?? "default";
  expect(result).toBe(0);
});

test("returns left-hand side for empty string", () => {
  const result = "" ?? "default";
  expect(result).toBe("");
});

test("returns left-hand side for false", () => {
  const result = false ?? "default";
  expect(result).toBe(false);
});

test("returns left-hand side for NaN", () => {
  const result = NaN ?? "default";
  expect(result).toBeNaN();
});

test("returns left-hand side for truthy values", () => {
  expect(42 ?? "default").toBe(42);
  expect("hello" ?? "default").toBe("hello");
  expect(true ?? "default").toBe(true);
});

test("nullish coalescing with object properties", () => {
  const config = { timeout: 0, name: "" };
  expect(config.timeout ?? 5000).toBe(0);
  expect(config.name ?? "anonymous").toBe("");
  expect(config.missing ?? "fallback").toBe("fallback");
});

test("chained nullish coalescing", () => {
  const a = null;
  const b = undefined;
  const c = "found";
  expect(a ?? b ?? c).toBe("found");
});

test("nullish coalescing vs logical OR", () => {
  // ?? only treats null/undefined as nullish
  // || treats all falsy values as falsy
  expect(0 ?? "default").toBe(0);
  expect(0 || "default").toBe("default");

  expect("" ?? "default").toBe("");
  expect("" || "default").toBe("default");

  expect(false ?? "default").toBe(false);
  expect(false || "default").toBe("default");

  // Both behave the same for null and undefined
  expect(null ?? "default").toBe("default");
  expect(null || "default").toBe("default");

  expect(undefined ?? "default").toBe("default");
  expect(undefined || "default").toBe("default");
});
