describe("mock semantics (vitest-documented behavior)", () => {
  test("mockResolvedValueOnce ordering with mockResolvedValue", async () => {
    const m = mock();
    m.mockResolvedValue("base").mockResolvedValueOnce("first").mockResolvedValueOnce("second");
    expect(await m()).toBe("first");
    expect(await m()).toBe("second");
    expect(await m()).toBe("base");
  });
  test("mockRejectedValue produces rejection", async () => {
    const m = mock();
    m.mockRejectedValue(new Error("nope"));
    await expect(m()).rejects.toThrow(/nope/);
  });
  test("mixing sync Once with resolved base", async () => {
    const m = mock();
    m.mockResolvedValue("async").mockReturnValueOnce("sync");
    expect(m()).toBe("sync");
    expect(await m()).toBe("async");
  });
  test("spy restore chain", () => {
    const o = { f: () => "orig" };
    const s = spyOn(o, "f").mockImplementation(() => "fake");
    expect(o.f()).toBe("fake");
    s.mockRestore();
    expect(o.f()).toBe("orig");
  });
  test("asymmetric matcher inside toHaveBeenCalledWith", () => {
    const m = mock();
    m({ id: "abc", n: 7 });
    expect(m).toHaveBeenCalledWith(expect.objectContaining({ id: expect.any(String) }));
  });
});
