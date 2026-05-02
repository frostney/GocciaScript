describe("String.prototype.codePointAt", () => {
  test("returns code point for ASCII characters", () => {
    expect("A".codePointAt(0)).toBe(65);
    expect("Z".codePointAt(0)).toBe(90);
    expect("0".codePointAt(0)).toBe(48);
  });

  test("returns code point at given position", () => {
    expect("ABC".codePointAt(1)).toBe(66);
    expect("ABC".codePointAt(2)).toBe(67);
  });

  test("returns undefined for out of bounds", () => {
    expect("ABC".codePointAt(3)).toBe(undefined);
    expect("ABC".codePointAt(-1)).toBe(undefined);
  });

  test("returns undefined for empty string", () => {
    expect("".codePointAt(0)).toBe(undefined);
  });

  test("indexes valid UTF-8 characters as ECMAScript string positions", () => {
    expect("é".codePointAt(0)).toBe(0xe9);
    expect("é".codePointAt(1)).toBe(undefined);
  });

  test("returns full code point for leading surrogate position", () => {
    expect("😀".codePointAt(0)).toBe(0x1f600);
    expect("😀".codePointAt(1)).toBe(0xde00);
  });

  test("returns lone surrogate code unit values", () => {
    expect("\uD800".codePointAt(0)).toBe(0xd800);
  });
});
