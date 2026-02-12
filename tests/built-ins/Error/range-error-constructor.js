/*---
description: RangeError constructor creates error objects correctly
features: [RangeError]
---*/

test("RangeError constructor", () => {
  const error = new RangeError("out of range");
  expect(error.message).toBe("out of range");
  expect(error.name).toBe("RangeError");
});

test("RangeError with empty message", () => {
  const error = new RangeError("");
  expect(error.message).toBe("");
  expect(error.name).toBe("RangeError");
});

test("RangeError can be thrown and caught", () => {
  let caught = null;
  try {
    throw new RangeError("invalid value");
  } catch (e) {
    caught = e;
  }
  expect(caught.name).toBe("RangeError");
  expect(caught.message).toBe("invalid value");
});
