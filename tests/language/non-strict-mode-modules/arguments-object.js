function capture() {
  return typeof arguments;
}

test("compat-non-strict-mode does not enable arguments in modules", () => {
  expect(capture("value")).toBe("undefined");
});
