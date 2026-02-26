describe("TypedArray.prototype.toString", () => {
  test("works like join", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.toString()).toBe("1,2,3");
  });

  test("on empty returns empty string", () => {
    expect(new Int32Array(0).toString()).toBe("");
  });
});
