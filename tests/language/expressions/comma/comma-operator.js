describe("comma operator", () => {
  test("evaluates operands left to right and returns the last value", () => {
    let log = "";
    const value = (log = log + "a", log = log + "b", 42);

    expect(log).toBe("ab");
    expect(value).toBe(42);
  });

  test("does not consume comma-separated call arguments", () => {
    const collect = (a, b) => [a, b];
    const values = collect(1, 2);

    expect(values[0]).toBe(1);
    expect(values[1]).toBe(2);
    expect(values.length).toBe(2);
  });

  test("parses arrow expression statements after statement-list entries", () => {
    {}() => 1, 42;
    class C {}() => 1, 42;

    expect(typeof C).toBe("function");
  });
});
