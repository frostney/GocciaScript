test("console.table exists and returns undefined", () => {
  expect(typeof console.table).toBe("function");
  expect(console.table([1, 2, 3])).toBeUndefined();
});
