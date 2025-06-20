/*---
description: Nullish coalescing operator (??) tests
features: [nullish-coalescing]
---*/

test("nullish coalescing with null and undefined", () => {
  expect(null ?? "default").toBe("default");
  expect(undefined ?? "default").toBe("default");
});

test("nullish coalescing does not trigger for other falsy values", () => {
  expect(0 ?? "default").toBe(0);
  expect("" ?? "default").toBe("");
  expect(false ?? "default").toBe(false);
  expect(NaN ?? "default").toBe(NaN);
});

test("nullish coalescing with truthy values", () => {
  expect("hello" ?? "default").toBe("hello");
  expect(42 ?? "default").toBe(42);
  expect(true ?? "default").toBe(true);
  expect([] ?? "default").toEqual([]);
  expect({} ?? "default").toEqual({});
});

test("nullish coalescing chaining", () => {
  expect(null ?? undefined ?? "final").toBe("final");
  expect(null ?? "middle" ?? "final").toBe("middle");
  expect("first" ?? "middle" ?? "final").toBe("first");
});

test("nullish coalescing with variables", () => {
  let a = null;
  let b = undefined;
  let c = "value";
  let d = 0;

  expect(a ?? "default").toBe("default");
  expect(b ?? "default").toBe("default");
  expect(c ?? "default").toBe("value");
  expect(d ?? "default").toBe(0);

  expect(a ?? b ?? c).toBe("value");
  expect(a ?? b ?? d).toBe(0);
});

test("nullish coalescing with expressions", () => {
  // Basic expression combinations
  expect((null ?? "backup") + " suffix").toBe("backup suffix");
  expect(null ?? false ?? "fallback").toBe(false);
  expect(undefined ?? null ?? "fallback").toBe("fallback");
});
