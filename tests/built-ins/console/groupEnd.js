test("console.groupEnd exists and returns undefined", () => {
  expect(typeof console.groupEnd).toBe("function");
  expect(console.groupEnd()).toBeUndefined();
});
