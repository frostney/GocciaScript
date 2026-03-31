test("console.time exists and returns undefined", () => {
  expect(typeof console.time).toBe("function");
  expect(console.time("test-timer")).toBeUndefined();
});
