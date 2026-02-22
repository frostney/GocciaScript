/*---
description: DOMException constructor behavior
features: [DOMException]
---*/

describe("DOMException constructor", () => {
  test("creates instance with default name 'Error' and code 0", () => {
    const e = new DOMException("something failed");
    expect(e instanceof DOMException).toBe(true);
    expect(e instanceof Error).toBe(true);
    expect(e.message).toBe("something failed");
    expect(e.name).toBe("Error");
    expect(e.code).toBe(0);
  });

  test("creates instance with custom name", () => {
    const e = new DOMException("bad data", "DataCloneError");
    expect(e.name).toBe("DataCloneError");
    expect(e.code).toBe(25);
    expect(e.message).toBe("bad data");
  });

  test("defaults message to empty string when not provided", () => {
    const e = new DOMException();
    expect(e.message).toBe("");
    expect(e.name).toBe("Error");
    expect(e.code).toBe(0);
  });

  test("unknown name gets code 0", () => {
    const e = new DOMException("msg", "CustomError");
    expect(e.name).toBe("CustomError");
    expect(e.code).toBe(0);
  });

  test("has stack property", () => {
    const e = new DOMException("test");
    expect(typeof e.stack).toBe("string");
  });

  test("prototype chain is correct", () => {
    const e = new DOMException("test");
    expect(e instanceof DOMException).toBe(true);
    expect(e instanceof Error).toBe(true);
  });
});
