test("console.timeEnd exists and returns undefined", () => {
  expect(typeof console.timeEnd).toBe("function");
  console.time("test-timer");
  expect(console.timeEnd("test-timer")).toBeUndefined();
});
