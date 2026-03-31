test("console.info exists and returns undefined", () => {
  expect(typeof console.info).toBe("function");
  expect(console.info("test info")).toBeUndefined();
});
