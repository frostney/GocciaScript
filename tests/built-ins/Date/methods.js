describe("Date methods", () => {
  // 2024-06-15T11:30:45.123Z (Saturday)
  const epoch = 1718451045123;
  const d = new Date(epoch);

  test("getTime and valueOf return epoch ms", () => {
    expect(d.getTime()).toBe(epoch);
    expect(d.valueOf()).toBe(epoch);
  });

  test("UTC getters", () => {
    expect(d.getUTCFullYear()).toBe(2024);
    expect(d.getUTCMonth()).toBe(5);
    expect(d.getUTCDate()).toBe(15);
    expect(d.getUTCDay()).toBe(6);
    expect(d.getUTCHours()).toBe(11);
    expect(d.getUTCMinutes()).toBe(30);
    expect(d.getUTCSeconds()).toBe(45);
    expect(d.getUTCMilliseconds()).toBe(123);
  });

  test("toISOString returns ISO 8601", () => {
    expect(d.toISOString()).toBe("2024-06-15T11:30:45.123Z");
  });

  test("toJSON returns same as toISOString", () => {
    expect(d.toJSON()).toBe(d.toISOString());
  });

  test("toString returns formatted string", () => {
    const str = d.toString();
    expect(str).toContain("2024");
    expect(str).toContain("GMT");
  });

  test("Date.now() returns a number", () => {
    expect(typeof Date.now()).toBe("number");
  });

  test("Date.parse() parses ISO string to epoch ms", () => {
    expect(Date.parse("2024-06-15T11:30:45.123Z")).toBe(epoch);
  });

  test("Date.UTC() returns an epoch millisecond timestamp", () => {
    expect(Date.UTC(2024, 5, 15, 11, 30, 45, 123)).toBe(epoch);
  });

  test("setDate() updates the local day and returns epoch ms", () => {
    const value = new Date(2024, 0, 1);
    const result = value.setDate(value.getDate() + 1);
    expect(result).toBe(value.getTime());
    expect(value.getDate()).toBe(2);
  });
});
