test("Object.is - Evaluation result is the same as using ===", () => {
  expect(Object.is(25, 25)).toBe(true);
  expect(Object.is("foo", "foo")).toBe(true);
  expect(Object.is("foo", "bar")).toBe(false);
  expect(Object.is(null, null)).toBe(true);
  expect(Object.is(undefined, undefined)).toBe(true);
  expect(Object.is([], [])).toBe(false);
  const foo = { a: 1 };
  const bar = { a: 1 };
  const sameFoo = foo;
  expect(Object.is(foo, foo)).toBe(true);
  expect(Object.is(foo, bar)).toBe(false);
  expect(Object.is(foo, sameFoo)).toBe(true);
});

test("Object.is - Signed zero", () => {
  expect(Object.is(0, -0)).toBe(false);
  expect(Object.is(+0, -0)).toBe(false);
  expect(Object.is(-0, -0)).toBe(true);
});

test("Object.is - NaN", () => {
  expect(Object.is(NaN, 0 / 0)).toBe(true);
  expect(Object.is(NaN, Number.NaN)).toBe(true);
});
