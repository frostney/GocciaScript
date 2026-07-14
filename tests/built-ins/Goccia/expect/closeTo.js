describe("expect.closeTo", () => {
  test("uses Vitest precision semantics", () => {
    expect(1.004).toEqual(expect.closeTo(1, 2));
    expect(1.006).not.toEqual(expect.closeTo(1, 2));
    expect(0.005).not.toEqual(expect.closeTo(0, 2));
    expect(Infinity).toEqual(expect.closeTo(Infinity));
    expect(-Infinity).toEqual(expect.closeTo(-Infinity));
    expect(1.006).toEqual(expect.not.closeTo(1, 2));
    expect(1).not.toEqual(expect.closeTo(1, Infinity));
    expect(1).not.toEqual(expect.closeTo(1, NaN));
    expect(1).not.toEqual(expect.closeTo(1, 1e100));
  });

  test("validates sample and precision types", () => {
    expect(() => expect.closeTo("1")).toThrow("Expected is not a Number");
    expect(() => expect.closeTo(1, "2")).toThrow(
      "Precision is not a Number",
    );
  });

  test("compares matcher state with SameValue semantics", () => {
    expect(NaN).toEqual(NaN);
    expect(-0).not.toEqual(0);
    expect(expect.closeTo(1, NaN)).toEqual(expect.closeTo(1, NaN));
    expect(expect.closeTo(1, -0)).not.toEqual(expect.closeTo(1, 0));
    expect(expect.objectContaining({ value: NaN })).toEqual(
      expect.objectContaining({ value: NaN }),
    );
  });
});
