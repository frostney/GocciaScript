describe("TypedArray.prototype.join", () => {
  test("join with default separator", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.join()).toBe("1,2,3");
  });

  test("join with custom separator", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.join(" - ")).toBe("1 - 2 - 3");
  });

  test("on empty returns empty string", () => {
    expect(new Int32Array(0).join()).toBe("");
  });

  test("single element has no separator", () => {
    expect(new Int32Array([42]).join()).toBe("42");
  });

  test("join with empty string separator", () => {
    expect(new Int32Array([1, 2, 3]).join("")).toBe("123");
  });

  test("Float64Array formats numbers", () => {
    expect(new Float64Array([1.5, 2.5]).join()).toBe("1.5,2.5");
  });
});
