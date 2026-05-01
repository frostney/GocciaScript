/*---
description: If statement conditions evaluate exactly once
features: [if-statement]
---*/

test("if condition side effects run once for true and false branches", () => {
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
});

test("postfix updates in if conditions run once", () => {
  let index = 0;

  if (index++ < 3) {
  }

  expect(index).toBe(1);
});
