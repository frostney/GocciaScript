test("console.debug exists and returns undefined", () => {
  expect(typeof console.debug).toBe("function");
  expect(console.debug("test debug")).toBeUndefined();
});
