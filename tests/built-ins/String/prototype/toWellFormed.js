describe("String.prototype.toWellFormed", () => {
  test("well-formed string is unchanged", () => {
    expect("hello".toWellFormed()).toBe("hello");
  });

  test("empty string is unchanged", () => {
    expect("".toWellFormed()).toBe("");
  });

  test("ASCII string is unchanged", () => {
    expect("abc123".toWellFormed()).toBe("abc123");
  });
});
