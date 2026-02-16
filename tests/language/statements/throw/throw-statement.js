/*---
description: throw statement with various value types
features: [throw]
---*/

describe("throw statement", () => {
  test("throw a string", () => {
    let caught = null;
    try {
      throw "string error";
    } catch (e) {
      caught = e;
    }
    expect(caught).toBe("string error");
  });

  test("throw a number", () => {
    let caught = null;
    try {
      throw 42;
    } catch (e) {
      caught = e;
    }
    expect(caught).toBe(42);
  });

  test("throw a boolean", () => {
    let caught = null;
    try {
      throw true;
    } catch (e) {
      caught = e;
    }
    expect(caught).toBe(true);
  });

  test("throw null", () => {
    let caught = "not set";
    try {
      throw null;
    } catch (e) {
      caught = e;
    }
    expect(caught).toBeNull();
  });

  test("throw an object", () => {
    let caught = null;
    try {
      throw { code: 404, message: "not found" };
    } catch (e) {
      caught = e;
    }
    expect(caught.code).toBe(404);
    expect(caught.message).toBe("not found");
  });

  test("throw an Error object", () => {
    let caught = null;
    try {
      throw new Error("something went wrong");
    } catch (e) {
      caught = e;
    }
    expect(caught.name).toBe("Error");
    expect(caught.message).toBe("something went wrong");
  });

  test("throw a TypeError", () => {
    let caught = null;
    try {
      throw new TypeError("invalid type");
    } catch (e) {
      caught = e;
    }
    expect(caught.name).toBe("TypeError");
    expect(caught.message).toBe("invalid type");
  });

  test("re-throwing in catch block", () => {
    let outerCaught = null;
    try {
      try {
        throw new Error("original");
      } catch (e) {
        throw e;
      }
    } catch (e) {
      outerCaught = e;
    }
    expect(outerCaught.message).toBe("original");
  });

  test("throw stops execution in try block", () => {
    let reached = false;
    try {
      throw "stop";
      reached = true;
    } catch (e) {
      // caught
    }
    expect(reached).toBe(false);
  });

  test("throw inside a function", () => {
    const throwError = () => {
      throw new Error("from function");
    };

    let caught = null;
    try {
      throwError();
    } catch (e) {
      caught = e;
    }
    expect(caught.message).toBe("from function");
  });

  test("throw with finally block", () => {
    let finallyRan = false;
    let caught = null;
    try {
      throw "error";
    } catch (e) {
      caught = e;
    } finally {
      finallyRan = true;
    }
    expect(caught).toBe("error");
    expect(finallyRan).toBe(true);
  });
});
