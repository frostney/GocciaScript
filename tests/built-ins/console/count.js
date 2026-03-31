test("console.count exists and returns undefined", () => {
  expect(typeof console.count).toBe("function");
  expect(console.count("test-label")).toBeUndefined();
  expect(console.count("test-label")).toBeUndefined();
});
