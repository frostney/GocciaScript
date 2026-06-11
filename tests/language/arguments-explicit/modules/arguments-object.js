function capture(first) {
  arguments[0] = "argument";
  first = "parameter";
  return [arguments.length, arguments[0], first, this];
}

describe("module arguments object compatibility", () => {
  test("enables unmapped arguments inside module functions", () => {
    expect(capture("value")).toEqual([1, "argument", "parameter", undefined]);
  });

  test("keeps module source strict while arguments are enabled", () => {
    expect(this).toBeUndefined();

    const obj = {};
    Object.defineProperty(obj, "fixed", {
      value: 1,
      writable: false,
      configurable: false
    });

    expect(() => {
      obj.fixed = 2;
    }).toThrow(TypeError);
    expect(() => {
      delete obj.fixed;
    }).toThrow(TypeError);
  });
});
