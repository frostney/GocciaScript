describe("toHaveBeenCalledWith", () => {
  test("matches a call by its arguments", () => {
    const fn = mock();
    fn(1, 2);

    expect(fn).toHaveBeenCalledWith(1, 2);
    expect(fn).not.toHaveBeenCalledWith(2, 1);
  });

  test("compares arguments deeply", () => {
    const fn = mock();
    fn({ a: [1, { b: 2 }] });

    expect(fn).toHaveBeenCalledWith({ a: [1, { b: 2 }] });
    expect(fn).not.toHaveBeenCalledWith({ a: [1, { b: 3 }] });
  });

  test("counts arguments, unlike array equality", () => {
    // toEqual ignores undefined items past the shorter length, but a call
    // still has to be made with the same number of arguments.
    const passedUndefined = mock();
    passedUndefined(undefined);
    expect(passedUndefined).not.toHaveBeenCalledWith();

    const passedNothing = mock();
    passedNothing();
    expect(passedNothing).not.toHaveBeenCalledWith(undefined);

    const passedTwo = mock();
    passedTwo(1, 2);
    expect(passedTwo).not.toHaveBeenCalledWith(1);
  });

  test("ignores undefined keys inside an argument", () => {
    const fn = mock();
    fn({ x: 1, y: undefined });

    expect(fn).toHaveBeenCalledWith({ x: 1 });
  });

  test("matches any recorded call", () => {
    const fn = mock();
    fn("first");
    fn("second");

    expect(fn).toHaveBeenCalledWith("first");
    expect(fn).toHaveBeenCalledWith("second");
    expect(fn).not.toHaveBeenCalledWith("third");
  });
});
