/*---
description: ReferenceError constructor creates error objects correctly
features: [ReferenceError]
---*/

test("ReferenceError constructor with message", () => {
  const error = new ReferenceError("not defined");
  expect(error.message).toBe("not defined");
  expect(error.name).toBe("ReferenceError");
});

test("ReferenceError with empty message", () => {
  const error = new ReferenceError("");
  expect(error.message).toBe("");
  expect(error.name).toBe("ReferenceError");
});

test("ReferenceError is instanceof Error", () => {
  const error = new ReferenceError("test");
  expect(error instanceof ReferenceError).toBe(true);
  expect(error instanceof Error).toBe(true);
});

test("ReferenceError can be thrown and caught", () => {
  let caught = null;
  try {
    throw new ReferenceError("x is not defined");
  } catch (e) {
    caught = e;
  }
  expect(caught.name).toBe("ReferenceError");
  expect(caught.message).toBe("x is not defined");
});

test("accessing undeclared variable throws ReferenceError", () => {
  let caughtName = "";
  try {
    const x = undeclaredVariable;
  } catch (e) {
    caughtName = e.name;
  }
  expect(caughtName).toBe("ReferenceError");
});
