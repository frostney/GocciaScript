/*---
description: The var head of a for statement inside a catch block assigns the catch parameter it redeclares (ES2026 B.3.4)
features: [compat-var, compat-traditional-for-loop]
---*/

test("the loop initializer and update assign the catch parameter", () => {
  var x = "var";
  const seen = [];
  let after;
  try {
    throw "thrown";
  } catch (x) {
    for (var x = 2; x < 4; x++) {
      seen.push(x);
    }
    after = x;
  }
  expect(seen).toEqual([2, 3]);
  expect(after).toBe(4);
  expect(x).toBe("var");
});
