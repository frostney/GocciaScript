/*---
description: Sloppy-mode for-in var bindings may be named let
features: [compat-for-in-loop, compat-var, compat-non-strict-mode]
---*/

test("var let is allowed in for-in heads", () => {
  let seen = 0;

  for (var let in { attr: null }) {
    expect(let).toBe("attr");
    seen += 1;
  }

  expect(seen).toBe(1);
});
