test("console.countReset exists and returns undefined", () => {
  expect(typeof console.countReset).toBe("function");
  expect(console.countReset("test-label")).toBeUndefined();
});
