test("console.clear exists and returns undefined", () => {
  expect(typeof console.clear).toBe("function");
  expect(console.clear()).toBeUndefined();
});
