describe("String.prototype.isWellFormed", () => {
  test("ASCII strings are well-formed", () => {
    expect("hello".isWellFormed()).toBe(true);
  });

  test("empty string is well-formed", () => {
    expect("".isWellFormed()).toBe(true);
  });

  test("regular strings are well-formed", () => {
    expect("abc123!@#".isWellFormed()).toBe(true);
  });
});
