describe("Date constructor", () => {
  test("typeof Date is function", () => {
    expect(typeof Date).toBe("function");
  });

  test("new Date() returns current time", () => {
    const before = Date.now();
    const d = new Date();
    const after = Date.now();
    expect(d.getTime() >= before).toBe(true);
    expect(d.getTime() <= after).toBe(true);
  });

  test("new Date(ms) creates from epoch milliseconds", () => {
    const d = new Date(0);
    expect(d.getTime()).toBe(0);
    expect(d.getUTCFullYear()).toBe(1970);
    expect(d.getUTCMonth()).toBe(0);
    expect(d.getUTCDate()).toBe(1);
  });

  test("new Date(string) parses ISO string", () => {
    const d = new Date("2024-06-15T12:30:00Z");
    expect(d.getUTCFullYear()).toBe(2024);
    expect(d.getUTCMonth()).toBe(5);
    expect(d.getUTCDate()).toBe(15);
    expect(d.getUTCHours()).toBe(12);
    expect(d.getUTCMinutes()).toBe(30);
  });

  test("new Date(year, month) with component args", () => {
    // Month is 0-indexed, so month=0 is January
    const d = new Date(2024, 0, 15, 0, 0, 0, 0);
    expect(d.getFullYear()).toBe(2024);
    expect(d.getMonth()).toBe(0);
    expect(d.getDate()).toBe(15);
  });

  test("year 0-99 maps to 1900-1999", () => {
    const d = new Date(95, 0, 1, 0, 0, 0, 0);
    expect(d.getFullYear()).toBe(1995);
  });
});
