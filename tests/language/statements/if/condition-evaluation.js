/*---
description: If statement conditions evaluate exactly once
features: [if-statement]
---*/

test("if condition side effects run once for true, false, and throwing paths", () => {
  let calls = 0;
  const pass = () => {
    calls = calls + 1;
    return true;
  };

  if (pass()) {
  }

  expect(calls).toBe(1);

  calls = 0;
  const fail = () => {
    calls = calls + 1;
    return false;
  };
  let branch = "";

  if (fail()) {
    branch = "then";
  } else {
    branch = "else";
  }

  expect(calls).toBe(1);
  expect(branch).toBe("else");

  calls = 0;
  const boom = () => {
    calls = calls + 1;
    throw new Error("boom");
  };
  let caught = false;

  try {
    if (boom()) {
    }
  } catch (error) {
    caught = error.message === "boom";
  }

  expect(caught).toBe(true);
  expect(calls).toBe(1);
});

test("postfix updates in if conditions run once", () => {
  let index = 0;

  if (index++ < 3) {
  }

  expect(index).toBe(1);
});
