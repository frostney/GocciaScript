test("console.error exists and returns undefined", () => {
  expect(typeof console.error).toBe("function");
  expect(console.error("test error")).toBeUndefined();
});
