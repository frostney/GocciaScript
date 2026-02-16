describe("try-catch-finally edge cases", () => {
  test("finally runs after return in try", () => {
    let finallyCalled = false;
    const fn = () => {
      try {
        return 42;
      } finally {
        finallyCalled = true;
      }
    };
    expect(fn()).toBe(42);
    expect(finallyCalled).toBe(true);
  });

  test("finally runs after throw in try", () => {
    let finallyCalled = false;
    try {
      try {
        throw "error";
      } finally {
        finallyCalled = true;
      }
    } catch (e) {
      // catch the rethrown error
    }
    expect(finallyCalled).toBe(true);
  });

  test("catch without parameter", () => {
    let caught = false;
    try {
      throw "error";
    } catch {
      caught = true;
    }
    expect(caught).toBe(true);
  });

  test("nested try-catch", () => {
    let result = "";
    try {
      try {
        throw "inner";
      } catch (e) {
        result = result + e;
        throw "outer";
      }
    } catch (e) {
      result = result + e;
    }
    expect(result).toBe("innerouter");
  });

  test("catch parameter shadows outer variable", () => {
    const e = "outer";
    try {
      throw "inner";
    } catch (e) {
      expect(e).toBe("inner");
    }
    expect(e).toBe("outer");
  });
});
