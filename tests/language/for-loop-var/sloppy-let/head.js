/*---
description: Sloppy-mode for heads treat let as an identifier when grammar permits
features: [compat-traditional-for-loop, compat-var, compat-non-strict-mode]
---*/

test("let can be an expression in traditional for heads", () => {
  var let;

  let = 1;
  for (let; ; )
    break;
  expect(let).toBe(1);

  let = 2;
  for (let = 3; ; )
    break;
  expect(let).toBe(3);

  let = 4;
  for ([let][0]; ; )
    break;
  expect(let).toBe(4);
});

test("var let is allowed in for-of heads", () => {
  let seen = 0;

  for (var let of [23]) {
    expect(let).toBe(23);
    seen += 1;
  }

  expect(seen).toBe(1);
});
