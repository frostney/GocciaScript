describe("parseInt", () => {
  test("typeof parseInt is function", () => {
    expect(typeof parseInt).toBe("function");
  });

  test("is the same function as Number.parseInt", () => {
    expect(parseInt).toBe(Number.parseInt);
  });
});
