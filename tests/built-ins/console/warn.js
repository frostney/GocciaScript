test("console.warn exists and returns undefined", () => {
  expect(typeof console.warn).toBe("function");
  expect(console.warn("test warning")).toBeUndefined();
});
