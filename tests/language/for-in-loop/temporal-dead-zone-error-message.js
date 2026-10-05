/*---
description: A for-in head that reads its own binding, or a const assigned as a for-in target, names it in the error
features: [compat-for-in-loop, temporal-dead-zone]
---*/

const messageOf = (run) => {
  try {
    run();
  } catch (error) {
    return `${error.constructor.name}: ${error.message}`;
  }
  return "no error";
};

describe("for-in error messages", () => {
  test("the head expression names the binding in its dead zone", () => {
    expect(messageOf(() => {
      for (const key in key) {
      }
    })).toBe("ReferenceError: Cannot access 'key' before initialization");
  });

  test("a const target names the const", () => {
    expect(messageOf(() => {
      const fixed = "a";
      for (fixed in { a: 1 }) {
      }
    })).toBe("TypeError: Assignment to constant variable 'fixed'");
  });
});
