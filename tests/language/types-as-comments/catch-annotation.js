/*---
description: Type annotations on catch parameters are parsed and ignored at runtime
features: [types-as-comments]
---*/

test("catch parameter with type annotation", () => {
  let caught = false;
  try {
    throw new Error("test");
  } catch (e: Error) {
    caught = true;
    expect(e.message).toBe("test");
  }
  expect(caught).toBe(true);
});

test("catch parameter with union type", () => {
  let msg = "";
  try {
    throw new Error("oops");
  } catch (e: Error | TypeError) {
    msg = e.message;
  }
  expect(msg).toBe("oops");
});

test("catch without type annotation still works", () => {
  let caught = false;
  try {
    throw new Error("plain");
  } catch (e) {
    caught = true;
  }
  expect(caught).toBe(true);
});
