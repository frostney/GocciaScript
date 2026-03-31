test("console.trace exists and returns undefined", () => {
  expect(typeof console.trace).toBe("function");
  expect(console.trace("test trace")).toBeUndefined();
});
