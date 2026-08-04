describe("mockRejectedValue", () => {
  test("returns a fresh rejected promise for every call", async () => {
    const reason = { code: "E_MOCK" };
    const fn = mock().mockRejectedValue(reason);
    const first = fn();
    const second = fn();

    expect(first).not.toBe(second);
    await expect(first).rejects.toBe(reason);
    await expect(second).rejects.toBe(reason);
  });

  test("mockRejectedValueOnce shares the one-shot queue", async () => {
    const fn = mock()
      .mockResolvedValue("default")
      .mockRejectedValueOnce("first rejection")
      .mockResolvedValueOnce("one-shot success");

    await expect(fn()).rejects.toBe("first rejection");
    await expect(fn()).resolves.toBe("one-shot success");
    await expect(fn()).resolves.toBe("default");
  });

  test("omitted values reject with undefined", async () => {
    await expect(mock().mockRejectedValue()()).rejects.toBeUndefined();
    await expect(mock().mockRejectedValueOnce()()).rejects.toBeUndefined();
  });
});
