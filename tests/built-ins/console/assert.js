test("console.assert exists and returns undefined", () => {
  expect(typeof console.assert).toBe("function");
  expect(console.assert(true, "should not print")).toBeUndefined();
  expect(console.assert(false, "should print")).toBeUndefined();
});
