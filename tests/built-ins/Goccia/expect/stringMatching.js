describe("expect.stringMatching", () => {
  test("matches strings and regular expressions", () => {
    expect("GocciaScript").toEqual(expect.stringMatching("Script$"));
    expect("gocciascript").toEqual(expect.stringMatching(/Goccia/i));
    expect("GocciaScript").toEqual(expect.not.stringMatching(/^Vitest/));
  });

  test("requires a string or regular expression sample", () => {
    expect(() => expect.stringMatching(1)).toThrow(
      "Expected is not a String or a RegExp",
    );
  });
});
