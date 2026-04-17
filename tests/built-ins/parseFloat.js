describe("parseFloat", () => {
  test("typeof parseFloat is function", () => {
    expect(typeof parseFloat).toBe("function");
  });

  test("is the same function as Number.parseFloat", () => {
    expect(parseFloat).toBe(Number.parseFloat);
  });
});
