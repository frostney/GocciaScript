describe("console methods", () => {
  test("console.log returns undefined", () => {
    expect(console.log("test")).toBeUndefined();
  });

  test("console.warn returns undefined", () => {
    expect(console.warn("test warning")).toBeUndefined();
  });

  test("console.error returns undefined", () => {
    expect(console.error("test error")).toBeUndefined();
  });

  test("console.info returns undefined", () => {
    expect(console.info("test info")).toBeUndefined();
  });

  test("console.debug returns undefined", () => {
    expect(console.debug("test debug")).toBeUndefined();
  });

  test("console.dir returns undefined", () => {
    expect(console.dir({ a: 1 })).toBeUndefined();
  });

  test("console.clear returns undefined", () => {
    expect(console.clear()).toBeUndefined();
  });

  test("console.trace returns undefined", () => {
    expect(console.trace("test trace")).toBeUndefined();
  });

  test("console.table returns undefined", () => {
    expect(console.table([1, 2, 3])).toBeUndefined();
  });

  test("console.group and groupEnd return undefined", () => {
    expect(console.group("group label")).toBeUndefined();
    expect(console.groupEnd()).toBeUndefined();
  });

  test("console.assert returns undefined", () => {
    expect(console.assert(true, "should not print")).toBeUndefined();
    expect(console.assert(false, "should print")).toBeUndefined();
  });

  test("console.count returns undefined", () => {
    expect(console.count("test-label")).toBeUndefined();
    expect(console.count("test-label")).toBeUndefined();
  });

  test("console.countReset returns undefined", () => {
    expect(console.countReset("test-label")).toBeUndefined();
  });

  test("console.time and timeEnd return undefined", () => {
    expect(console.time("test-timer")).toBeUndefined();
    expect(console.timeEnd("test-timer")).toBeUndefined();
  });

  test("console.timeLog returns undefined", () => {
    console.time("log-timer");
    expect(console.timeLog("log-timer")).toBeUndefined();
    console.timeEnd("log-timer");
  });

  test("all console methods exist", () => {
    expect(typeof console.log).toBe("function");
    expect(typeof console.warn).toBe("function");
    expect(typeof console.error).toBe("function");
    expect(typeof console.info).toBe("function");
    expect(typeof console.debug).toBe("function");
    expect(typeof console.dir).toBe("function");
    expect(typeof console.assert).toBe("function");
    expect(typeof console.count).toBe("function");
    expect(typeof console.countReset).toBe("function");
    expect(typeof console.time).toBe("function");
    expect(typeof console.timeEnd).toBe("function");
    expect(typeof console.timeLog).toBe("function");
    expect(typeof console.clear).toBe("function");
    expect(typeof console.group).toBe("function");
    expect(typeof console.groupEnd).toBe("function");
    expect(typeof console.trace).toBe("function");
    expect(typeof console.table).toBe("function");
  });
});
