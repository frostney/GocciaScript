test("console.log exists and returns undefined", () => {
  expect(typeof console.log).toBe("function");
  expect(console.log("test")).toBeUndefined();
});
