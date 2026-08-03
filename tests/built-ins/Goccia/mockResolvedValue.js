describe("mockResolvedValue", () => {
  test("returns a fresh fulfilled promise for every call", async () => {
    const fn = mock().mockResolvedValue(42);
    const first = fn();
    const second = fn();

    expect(first).not.toBe(second);
    await expect(first).resolves.toBe(42);
    await expect(second).resolves.toBe(42);
  });

  test("mockResolvedValueOnce shares the one-shot queue", async () => {
    const fn = mock()
      .mockResolvedValue("default")
      .mockResolvedValueOnce("first")
      .mockReturnValueOnce("raw");

    await expect(fn()).resolves.toBe("first");
    expect(fn()).toBe("raw");
    await expect(fn()).resolves.toBe("default");
  });

  test("omitted values resolve to undefined", async () => {
    await expect(mock().mockResolvedValue()()).resolves.toBeUndefined();
    await expect(mock().mockResolvedValueOnce()()).resolves.toBeUndefined();
  });
});
