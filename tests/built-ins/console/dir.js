test("console.dir exists and returns undefined", () => {
  expect(typeof console.dir).toBe("function");
  expect(console.dir({ a: 1 })).toBeUndefined();
});
