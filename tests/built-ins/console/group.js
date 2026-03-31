test("console.group exists and returns undefined", () => {
  expect(typeof console.group).toBe("function");
  expect(console.group("group label")).toBeUndefined();
});
