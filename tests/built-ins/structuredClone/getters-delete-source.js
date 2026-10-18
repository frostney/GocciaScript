/*---
description: structuredClone reads a getter and an error message that delete themselves from the source
features: [structuredClone]
---*/

test("a getter that deletes its own property is cloned as its result", () => {
  const source = {
    get x() {
      delete this.x;
      return 1;
    },
    z: 2,
  };
  const clone = structuredClone(source);
  expect(clone).toEqual({ x: 1, z: 2 });
  expect("x" in source).toBe(false);
});

test("an error message whose toString deletes the message", () => {
  const error = new Error("m");
  error.message = {
    toString() {
      delete error.message;
      return "y";
    },
  };
  const clone = structuredClone(error);
  expect(clone.message).toBe("y");
  expect(Object.hasOwn(error, "message")).toBe(false);
});
