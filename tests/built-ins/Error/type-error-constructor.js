/*---
description: TypeError constructor creates error objects correctly
features: [TypeError]
---*/

test("TypeError constructor with message", () => {
  const error = new TypeError("wrong type");
  expect(error.message).toBe("wrong type");
  expect(error.name).toBe("TypeError");
});

test("TypeError with empty message", () => {
  const error = new TypeError("");
  expect(error.message).toBe("");
  expect(error.name).toBe("TypeError");
});

test("TypeError is instanceof Error", () => {
  const error = new TypeError("test");
  expect(error instanceof TypeError).toBe(true);
  expect(error instanceof Error).toBe(true);
});

test("TypeError can be thrown and caught", () => {
  let caught = null;
  try {
    throw new TypeError("invalid type");
  } catch (e) {
    caught = e;
  }
  expect(caught.name).toBe("TypeError");
  expect(caught.message).toBe("invalid type");
});

test("TypeError can be caught selectively with instanceof", () => {
  let isTypeError = false;
  try {
    throw new TypeError("type issue");
  } catch (e) {
    isTypeError = e instanceof TypeError;
  }
  expect(isTypeError).toBe(true);
});

test("const reassignment throws TypeError", () => {
  let caughtName = "";
  try {
    const x = 1;
    x = 2;
  } catch (e) {
    caughtName = e.name;
  }
  expect(caughtName).toBe("TypeError");
});
