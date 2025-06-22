test("implicit global declarations are not allowed", () => {
  expect(() => {
    a = 1;
  }).toThrow();
});
