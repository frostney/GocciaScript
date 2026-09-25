describe("toHaveBeenCalledOnce", () => {
  test("passes for exactly one mock call", () => {
    const fn = mock();
    fn("value");

    expect(fn).toHaveBeenCalledOnce();
  });

  test("supports negation for zero and multiple calls", () => {
    const fn = mock();
    expect(fn).not.toHaveBeenCalledOnce();

    fn();
    fn();
    expect(fn).not.toHaveBeenCalledOnce();
  });

  test("works with spies", () => {
    const target = { run: () => "ok" };
    const spy = spyOn(target, "run");

    target.run();
    expect(spy).toHaveBeenCalledOnce();
  });

  test("rejects unexpected arguments", () => {
    const fn = mock();
    fn();

    expect(() => expect(fn).toHaveBeenCalledOnce("unexpected")).toThrow();
  });
});
