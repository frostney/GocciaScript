describe("expect.stringContaining", () => {
  test("matches string substrings", () => {
    expect("GocciaScript").toEqual(expect.stringContaining("Script"));
    expect("GocciaScript").not.toEqual(expect.stringContaining("Vitest"));
    expect("GocciaScript").toEqual(expect.not.stringContaining("Vitest"));
  });

  test("requires a string sample", () => {
    expect(() => expect.stringContaining(1)).toThrow(
      "Expected is not a string",
    );
  });
});
