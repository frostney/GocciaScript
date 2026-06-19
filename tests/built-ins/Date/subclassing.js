describe("Date subclassing", () => {
  test("subclass constructor initializes Date state", () => {
    class MyDate extends Date {}

    const local = new MyDate(1859, "10", 24, 11);
    expect(local instanceof MyDate).toBe(true);
    expect(local instanceof Date).toBe(true);
    expect(local.getFullYear()).toBe(1859);
    expect(local.getMonth()).toBe(10);
    expect(local.getDate()).toBe(24);

    const utc = new MyDate(-3474558000000);
    expect(utc.getUTCFullYear()).toBe(1859);
    expect(utc.getUTCMonth()).toBe(10);
    expect(utc.getUTCDate()).toBe(24);
  });
});
